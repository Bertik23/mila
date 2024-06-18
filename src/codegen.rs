use std::{
    cell::RefCell,
    collections::{HashMap, LinkedList},
};

use inkwell::{
    basic_block::BasicBlock,
    context,
    module::Linkage,
    values::{BasicValue, FunctionValue, IntValue, PointerValue},
    AddressSpace, IntPredicate,
};

use crate::parser::*;

struct Compiler<'ctx, 'a> {
    ctx: &'ctx context::Context,
    module: &'a inkwell::module::Module<'ctx>,
    builder: &'a inkwell::builder::Builder<'ctx>,
    variables: &'a mut LinkedList<HashMap<String, PointerValue<'ctx>>>,
    function: Option<FunctionValue<'ctx>>,
    break_to_block: RefCell<LinkedList<BasicBlock<'ctx>>>,
}

pub fn codegen(program: Program, outfile: &str) {
    let inkwell_context = context::Context::create();
    let module = inkwell_context.create_module(program.name.as_str());
    let mut ctx = Compiler {
        ctx: &inkwell_context,
        module: &module,
        builder: &inkwell_context.create_builder(),
        variables: &mut LinkedList::new(),
        function: None,
        break_to_block: RefCell::new(LinkedList::new()),
    };
    ctx.codegen(program);
    module.print_to_file(outfile).unwrap();
}

enum Values<'a> {
    Int(IntValue<'a>),
    Ptr(PointerValue<'a>),
    Nop,
    Ret,
    Break,
}

impl<'ctx, 'a> Compiler<'ctx, 'a> {
    fn codegen(&'a mut self, program: Program) {
        let write_fn_type = self
            .ctx
            .i32_type()
            .fn_type(&[self.ctx.i32_type().into()], false);
        let read_fn_type = self.ctx.i32_type().fn_type(
            &[self.ctx.i32_type().ptr_type(AddressSpace::default()).into()],
            false,
        );
        self.module.add_function(
            "writeln",
            write_fn_type,
            Some(Linkage::External),
        );
        self.module.add_function(
            "write",
            write_fn_type,
            Some(Linkage::External),
        );
        self.module.add_function(
            "readln",
            read_fn_type,
            Some(Linkage::External),
        );
        self.module
            .add_function("inc", read_fn_type, Some(Linkage::External));
        self.module
            .add_function("dec", read_fn_type, Some(Linkage::External));

        self.variables.push_back(HashMap::new());
        for glob in program.globals {
            let t = self.create_type(&glob.typ);
            let g = self.module.add_global(
                t,
                Some(AddressSpace::default()),
                glob.ident.as_str(),
            );
            if let Some(Expression::Literal(init_val)) = glob.assign {
                g.set_initializer(
                    &self.ctx.i32_type().const_int(init_val as u64, true),
                );
            } else {
                g.set_initializer(&self.ctx.i32_type().const_zero());
            }
            self.variables
                .back_mut()
                .unwrap()
                .insert(glob.ident.clone(), g.as_pointer_value());
        }

        for func in &program.functions {
            let fn_type = self.create_type(&func.typ).fn_type(
                func.params
                    .iter()
                    .map(|x| self.create_type(&x.typ).into())
                    .collect::<Vec<inkwell::types::BasicMetadataTypeEnum>>()
                    .as_slice(),
                false,
            );
            self.module.add_function(func.ident.as_str(), fn_type, None);
        }
        for func in program.functions {
            self.codegen_fndec(func);
        }
        let main_dec = FnDec {
            ident: "main".to_string(),
            typ: Type::Integer,
            params: vec![],
            locals: program.main_vars,
            expr: program.main_expr,
        };
        let main_fn_type = self.create_type(&main_dec.typ).fn_type(
            main_dec
                .params
                .iter()
                .map(|x| self.create_type(&x.typ).into())
                .collect::<Vec<inkwell::types::BasicMetadataTypeEnum>>()
                .as_slice(),
            false,
        );
        self.module
            .add_function(main_dec.ident.as_str(), main_fn_type, None);
        println!("Codegen for main:");
        self.codegen_fndec(main_dec);
    }

    fn codegen_fndec(&mut self, func: FnDec) {
        let fun = self.module.get_function(func.ident.as_str()).unwrap();
        self.function = Some(fun);
        let entry_block = self.ctx.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry_block);
        self.variables.push_back(HashMap::from([(
            func.ident.clone(),
            self.builder
                .build_alloca(self.create_type(&func.typ), func.ident.as_str())
                .unwrap(),
        )]));
        for (n, var) in func.params.iter().enumerate() {
            let ptr = self
                .builder
                .build_alloca(self.create_type(&var.typ), var.ident.as_str())
                .unwrap();
            self.builder
                .build_store(
                    ptr,
                    fun.get_nth_param(n as u32).unwrap().into_int_value(),
                )
                .unwrap();
            self.variables
                .back_mut()
                .unwrap()
                .insert(var.ident.clone(), ptr);
        }
        for loc in func.locals {
            self.codegen_vardec(loc);
        }
        #[cfg(not(return_expr))]
        {
            self.codegen_expr(func.expr);
            self.codegen_expr(Expression::Exit);
        }
        #[cfg(return_expr)]
        self.builder.build_return(Some(
            &self.load_val(self.codegen_expr(func.expr)).unwrap(),
        ));
        self.variables.pop_back();
    }

    fn codegen_expr(&self, expr: Expression) -> Values {
        match expr {
            Expression::Exit => {
                let r = self.function.unwrap();
                let l = self
                    .builder
                    .build_load(
                        self.function
                            .unwrap()
                            .get_type()
                            .get_return_type()
                            .unwrap(),
                        self.get_var(
                            &r.get_name().to_str().unwrap().to_string(),
                        )
                        .unwrap(),
                        "ret",
                    )
                    .unwrap();
                self.builder.build_return(Some(&l)).unwrap();
                Values::Ret
            }
            Expression::Var(ident) => {
                Values::Ptr(self.get_var(&ident).unwrap())
            }
            Expression::Literal(val) => {
                Values::Int(self.ctx.i32_type().const_int(val as u64, true))
            }
            Expression::Op(op, lhs, rhs) => {
                let l = self.codegen_expr(*lhs);
                let r = self.codegen_expr(*rhs);
                println!("Ahojda");
                match op {
                    Operation::Add => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        Values::Int(
                            self.builder.build_int_add(l, r, "add").unwrap(),
                        )
                    }
                    Operation::Sub => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        Values::Int(
                            self.builder.build_int_sub(l, r, "sub").unwrap(),
                        )
                    }
                    Operation::Mul => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        Values::Int(
                            self.builder.build_int_mul(l, r, "mul").unwrap(),
                        )
                    }
                    Operation::Div => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        Values::Int(
                            self.builder.build_int_mul(l, r, "mul").unwrap(),
                        )
                    }
                    Operation::Mod => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();

                        let div = self
                            .builder
                            .build_int_signed_div(l, r, "mod_div")
                            .unwrap();
                        let mul = self
                            .builder
                            .build_int_mul(div, r, "mod_mul")
                            .unwrap();
                        let sub = self
                            .builder
                            .build_int_sub(l, mul, "mod_sub")
                            .unwrap();
                        Values::Int(sub)
                    }
                    Operation::And => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        Values::Int(
                            self.builder.build_int_mul(l, r, "and").unwrap(),
                        )
                    }
                    Operation::Or => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        Values::Int(
                            self.builder.build_int_add(l, r, "or").unwrap(),
                        )
                    }
                    Operation::Eq => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, l, r, "neq")
                            .unwrap();
                        Values::Int(
                            self.builder
                                .build_cast(
                                    inkwell::values::InstructionOpcode::SExt,
                                    cmp,
                                    self.ctx.i32_type(),
                                    "cast_cmp_to_i32",
                                )
                                .unwrap()
                                .into_int_value(),
                        )
                    }
                    Operation::Neq => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::NE, l, r, "neq")
                            .unwrap();
                        Values::Int(
                            self.builder
                                .build_cast(
                                    inkwell::values::InstructionOpcode::SExt,
                                    cmp,
                                    self.ctx.i32_type(),
                                    "cast_cmp_to_i32",
                                )
                                .unwrap()
                                .into_int_value(),
                        )
                    }
                    Operation::Grater => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        let cmp = self
                            .builder
                            .build_int_compare(
                                IntPredicate::SGT,
                                l,
                                r,
                                "greater",
                            )
                            .unwrap();
                        Values::Int(
                            self.builder
                                .build_cast(
                                    inkwell::values::InstructionOpcode::SExt,
                                    cmp,
                                    self.ctx.i32_type(),
                                    "cast_cmp_to_i32",
                                )
                                .unwrap()
                                .into_int_value(),
                        )
                    }
                    Operation::Geq => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGE, l, r, "geq")
                            .unwrap();
                        Values::Int(
                            self.builder
                                .build_cast(
                                    inkwell::values::InstructionOpcode::SExt,
                                    cmp,
                                    self.ctx.i32_type(),
                                    "cast_cmp_to_i32",
                                )
                                .unwrap()
                                .into_int_value(),
                        )
                    }
                    Operation::Less => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLT, l, r, "less")
                            .unwrap();
                        Values::Int(
                            self.builder
                                .build_cast(
                                    inkwell::values::InstructionOpcode::SExt,
                                    cmp,
                                    self.ctx.i32_type(),
                                    "cast_cmp_to_i32",
                                )
                                .unwrap()
                                .into_int_value(),
                        )
                    }
                    Operation::Leq => {
                        let l = self.load_val(l).unwrap();
                        let r = self.load_val(r).unwrap();
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLE, l, r, "leq")
                            .unwrap();
                        Values::Int(
                            self.builder
                                .build_cast(
                                    inkwell::values::InstructionOpcode::SExt,
                                    cmp,
                                    self.ctx.i32_type(),
                                    "cast_cmp_to_i32",
                                )
                                .unwrap()
                                .into_int_value(),
                        )
                    }
                    Operation::Assign => {
                        let Values::Ptr(l) = l else { panic!() };
                        let r = match r {
                            Values::Int(r) => r,
                            _ => self.load_val(r).unwrap(),
                        };
                        self.builder.build_store(l, r).unwrap();
                        Values::Int(r)
                    }
                }
            }
            Expression::Block(exprs) => {
                let mut last = Values::Nop;
                for e in exprs {
                    last = self.codegen_expr(e);
                    if let Values::Ret = last {
                        return last;
                    }
                    if let Values::Break = last {
                        return Values::Nop;
                    }
                }
                last
            }
            Expression::Call(ident, params) => {
                if ident == "readln" || ident == "dec" || ident == "inc" {
                    let Values::Ptr(p) = self.codegen_expr(params[0].clone())
                    else {
                        panic!("Invalid type.")
                    };
                    return Values::Int(
                        self.builder
                            .build_call(
                                self.module
                                    .get_function(ident.as_str())
                                    .unwrap(),
                                &[p.into()],
                                "call ptr",
                            )
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                            .into_int_value(),
                    );
                }
                Values::Int(self.builder.build_call(
                    self.module.get_function(ident.as_str()).unwrap(),
                    &params
                        .into_iter()
                        .map(|x| self.load_val(self.codegen_expr(x)).unwrap().into())
                        .collect::<Vec<inkwell::values::BasicMetadataValueEnum>>(),
                    "call",
                ).unwrap().try_as_basic_value().left().unwrap().into_int_value())
            }
            Expression::If { cond, then, el } => {
                let bb = self.builder.get_insert_block().unwrap();
                let c = self.load_val(self.codegen_expr(*cond)).unwrap();
                let c = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        c,
                        self.ctx.i32_type().const_zero(),
                        "notzerocmp",
                    )
                    .unwrap();
                let then_block =
                    self.ctx.append_basic_block(self.function.unwrap(), "then");
                let else_block =
                    self.ctx.append_basic_block(self.function.unwrap(), "else");
                let after_block = self
                    .ctx
                    .append_basic_block(self.function.unwrap(), "after");

                let mut incoming = Vec::new();
                self.builder.position_at_end(then_block);
                let t = self.codegen_expr(*then);
                if let Values::Ret = t {
                } else if let Values::Break = t {
                } else {
                    let t = self.load_val(t).unwrap().as_basic_value_enum();
                    incoming.push((t, then_block));
                    self.builder
                        .build_unconditional_branch(after_block)
                        .unwrap();
                }

                self.builder.position_at_end(else_block);
                let e = self
                    .codegen_expr(*el.unwrap_or(Box::new(Expression::Nothing)));
                if let Values::Ret = e {
                } else if let Values::Break = e {
                } else {
                    let e = self.load_val(e).unwrap().as_basic_value_enum();
                    incoming.push((e, else_block));
                    self.builder
                        .build_unconditional_branch(after_block)
                        .unwrap();
                }

                self.builder.position_at_end(bb);
                self.builder
                    .build_conditional_branch(c, then_block, else_block)
                    .unwrap();
                self.builder.position_at_end(after_block);
                let phi = self
                    .builder
                    .build_phi(self.ctx.i32_type(), "phi_if-else")
                    .unwrap();
                let incoming: Vec<(&dyn BasicValue, _)> = incoming
                    .iter()
                    .map(|(value, block)| (value as &dyn BasicValue, *block))
                    .collect();
                phi.add_incoming(&incoming);
                Values::Int(phi.as_basic_value().into_int_value())
            }
            Expression::While { cond, body } => {
                let cond_bb = self.ctx.append_basic_block(
                    self.function.unwrap(),
                    "while_condition",
                );
                let body_bb = self
                    .ctx
                    .append_basic_block(self.function.unwrap(), "while_body");
                let after_bb = self
                    .ctx
                    .append_basic_block(self.function.unwrap(), "while_after");

                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(cond_bb);
                let c = self.load_val(self.codegen_expr(*cond)).unwrap();
                let c = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        c,
                        self.ctx.i32_type().const_zero(),
                        "notzerocmp",
                    )
                    .unwrap();
                self.builder
                    .build_conditional_branch(c, body_bb, after_bb)
                    .unwrap();

                self.builder.position_at_end(body_bb);
                self.break_to_block.borrow_mut().push_back(after_bb);
                let b = self.codegen_expr(*body);
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(after_bb);
                self.break_to_block.borrow_mut().pop_back().unwrap();
                b
            }
            Expression::For => todo!(),
            Expression::Break => {
                self.builder
                    .build_unconditional_branch(
                        *self.break_to_block.borrow().back().unwrap(),
                    )
                    .unwrap();
                Values::Break
            }
            Expression::Nothing => {
                Values::Int(self.ctx.i32_type().const_int(0, false))
            } // e => todo!("{:?}", e),
        }
    }

    fn create_type(&self, _typ: &Type) -> inkwell::types::IntType<'ctx> {
        self.ctx.i32_type()
    }

    fn get_var(
        &self,
        ident: &String,
    ) -> Result<inkwell::values::PointerValue, String> {
        for vars in self.variables.iter().rev() {
            if let Some(r) = vars.get(ident) {
                return Ok(*r);
            }
        }
        dbg!(&self.variables);
        Err(format!("Variable '{}' not found.", ident))
    }

    fn load_val<'b, 'm>(&'b self, ptr: Values<'m>) -> anyhow::Result<IntValue>
    where
        'm: 'b,
    {
        match ptr {
            Values::Ptr(p) => Ok(self
                .builder
                .build_load(self.ctx.i32_type(), p, "load_val")?
                .into_int_value()),
            Values::Int(i) => Ok(i),
            _ => Ok(self.ctx.i32_type().const_zero()),
        }
    }

    fn codegen_vardec(&mut self, var: VarDec) {
        let t = self.create_type(&var.typ);
        self.variables.back_mut().unwrap().insert(
            var.ident.clone(),
            self.builder.build_alloca(t, var.ident.as_str()).unwrap(),
        );
        if let Some(ass) = var.assign {
            self.codegen_expr(Expression::Op(
                Operation::Assign,
                Box::new(Expression::Var(var.ident)),
                Box::new(ass),
            ));
        }
    }
}
