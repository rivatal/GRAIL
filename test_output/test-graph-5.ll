; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %0 = alloca { i32 }
  %ptr = getelementptr inbounds { i32 }* %0, i32 0, i32 0
  store i32 1, i32* %ptr
  %1 = load { i32 }* %0
  %g = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  %ptr1 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 2
  store { i32 } %1, { i32 }* %ptr1
  %2 = alloca { i32 }
  %ptr2 = getelementptr inbounds { i32 }* %2, i32 0, i32 0
  store i32 1, i32* %ptr2
  %3 = load { i32 }* %2
  %strct = alloca { { i32 }*, i32 }
  %lst = alloca { i32 }
  %ptr3 = getelementptr inbounds { i32 }* %lst, i32 0
  store { i32 } %3, { i32 }* %ptr3
  %p0 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 1
  store { i32 }* %lst, { i32 }** %p0
  store i32 1, i32* %p1
  %lst4 = load { { i32 }*, i32 }* %strct
  %strct5 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %lst6 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 0
  %p07 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct5, i32 0, i32 0
  %p18 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct5, i32 0, i32 1
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst6, { { i32 }*, { i32 }*, i1, { i32 } }** %p07
  store i32 0, i32* %p18
  %lst9 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct5
  %ptr10 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 0
  store { { i32 }*, i32 } %lst4, { { i32 }*, i32 }* %ptr10
  %ptr11 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %lst9, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr11
  %g12 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g
  %g13 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g12, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g13
  %i = alloca i32
  store i32 2, i32* %i
  br label %while

while:                                            ; preds = %merge91, %entry
  %i94 = load i32* %i
  %tmp95 = icmp sle i32 %i94, 5
  br i1 %tmp95, label %while_body, label %merge96

while_body:                                       ; preds = %while
  %i14 = load i32* %i
  %4 = alloca { i32 }
  %ptr15 = getelementptr inbounds { i32 }* %4, i32 0, i32 0
  store i32 %i14, i32* %ptr15
  %5 = load { i32 }* %4
  %mynode = alloca { i32 }
  store { i32 } %5, { i32 }* %mynode
  %g16 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g13
  %mynode17 = load { i32 }* %mynode
  %strct18 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g16, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct18
  %ptr19 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct18, i32 0, i32 0
  %nodes = load { { i32 }*, i32 }* %ptr19
  %strct20 = alloca { { i32 }*, i32 }
  %strct21 = alloca { { i32 }*, i32 }
  store { { i32 }*, i32 } %nodes, { { i32 }*, i32 }* %strct21
  %tmp = getelementptr inbounds { { i32 }*, i32 }* %strct21, i32 0, i32 1
  %len = load i32* %tmp
  %tmp22 = getelementptr inbounds { { i32 }*, i32 }* %strct21, i32 0, i32 0
  %lst23 = load { i32 }** %tmp22
  %len24 = add i32 1, %len
  %tmp25 = getelementptr inbounds { { i32 }*, i32 }* %strct20, i32 0, i32 1
  store i32 %len24, i32* %tmp25
  %lst26 = alloca { i32 }, i32 %len24
  %ptr27 = getelementptr inbounds { i32 }* %lst26, i32 %len
  store { i32 } %mynode17, { i32 }* %ptr27
  %ind = alloca i32
  store i32 0, i32* %ind
  br label %checklimits

checklimits:                                      ; preds = %assignment, %while_body
  %tmp33 = load i32* %ind
  %comp = icmp slt i32 %tmp33, %len
  br i1 %comp, label %assignment, label %merge

assignment:                                       ; preds = %checklimits
  %i28 = load i32* %ind
  %ptr29 = getelementptr inbounds { i32 }* %lst23, i32 %i28
  %ptr30 = getelementptr inbounds { i32 }* %lst26, i32 %i28
  %tmp31 = load { i32 }* %ptr29
  store { i32 } %tmp31, { i32 }* %ptr30
  %tmp32 = load i32* %ind
  %inc = add i32 %tmp32, 1
  store i32 %inc, i32* %ind
  br label %checklimits

merge:                                            ; preds = %checklimits
  %tmp34 = getelementptr inbounds { { i32 }*, i32 }* %strct20, i32 0, i32 0
  store { i32 }* %lst26, { i32 }** %tmp34
  %strct35 = load { { i32 }*, i32 }* %strct20
  %tmp36 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct18, i32 0, i32 0
  store { { i32 }*, i32 } %strct35, { { i32 }*, i32 }* %tmp36
  %g37 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct18
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g37, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g13
  %lst38 = alloca { { i32 }*, i32 }
  %g39 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g13
  %e = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g39, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %e
  %ptr40 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %e, i32 0, i32 0
  %nodes41 = load { { i32 }*, i32 }* %ptr40
  store { { i32 }*, i32 } %nodes41, { { i32 }*, i32 }* %lst38
  %p = alloca { i32 }
  %tmp42 = getelementptr inbounds { { i32 }*, i32 }* %lst38, i32 0, i32 0
  %ar = load { i32 }** %tmp42
  %tmp43 = getelementptr inbounds { { i32 }*, i32 }* %lst38, i32 0, i32 1
  %end = load i32* %tmp43
  %ind44 = alloca i32
  store i32 0, i32* %ind44
  br label %while45

while45:                                          ; preds = %merge82, %merge
  %tmp89 = load i32* %ind44
  %comp90 = icmp slt i32 %tmp89, %end
  br i1 %comp90, label %while_body46, label %merge91

while_body46:                                     ; preds = %while45
  %i47 = load i32* %ind44
  %ptr48 = getelementptr inbounds { i32 }* %ar, i32 %i47
  %tmp49 = load { i32 }* %ptr48
  store { i32 } %tmp49, { i32 }* %p
  %g50 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g13
  %g51 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g50, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g51
  %ptr52 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g51, i32 0, i32 2
  %tmp53 = load { i32 }* %ptr52
  %6 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr54 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %6, i32 0, i32 0
  store { i32 }* %mynode, { i32 }** %ptr54
  %ptr55 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %6, i32 0, i32 1
  store { i32 }* %p, { i32 }** %ptr55
  %ptr56 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %6, i32 0, i32 2
  store i1 false, i1* %ptr56
  %ptr57 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %6, i32 0, i32 3
  store { i32 } %tmp53, { i32 }* %ptr57
  %7 = load { { i32 }*, { i32 }*, i1, { i32 } }* %6
  %strct58 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g50, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct58
  %ptr59 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct58, i32 0, i32 1
  %nodes60 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr59
  %strct61 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %strct62 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %nodes60, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct62
  %tmp63 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct62, i32 0, i32 1
  %len64 = load i32* %tmp63
  %tmp65 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct62, i32 0, i32 0
  %lst66 = load { { i32 }*, { i32 }*, i1, { i32 } }** %tmp65
  %len67 = add i32 1, %len64
  %tmp68 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct61, i32 0, i32 1
  store i32 %len67, i32* %tmp68
  %lst69 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 %len67
  %ptr70 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst69, i32 %len64
  store { { i32 }*, { i32 }*, i1, { i32 } } %7, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr70
  %ind71 = alloca i32
  store i32 0, i32* %ind71
  br label %checklimits72

checklimits72:                                    ; preds = %assignment73, %while_body46
  %tmp80 = load i32* %ind71
  %comp81 = icmp slt i32 %tmp80, %len64
  br i1 %comp81, label %assignment73, label %merge82

assignment73:                                     ; preds = %checklimits72
  %i74 = load i32* %ind71
  %ptr75 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst66, i32 %i74
  %ptr76 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst69, i32 %i74
  %tmp77 = load { { i32 }*, { i32 }*, i1, { i32 } }* %ptr75
  store { { i32 }*, { i32 }*, i1, { i32 } } %tmp77, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr76
  %tmp78 = load i32* %ind71
  %inc79 = add i32 %tmp78, 1
  store i32 %inc79, i32* %ind71
  br label %checklimits72

merge82:                                          ; preds = %checklimits72
  %tmp83 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct61, i32 0, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst69, { { i32 }*, { i32 }*, i1, { i32 } }** %tmp83
  %strct84 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct61
  %tmp85 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct58, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %strct84, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %tmp85
  %g86 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct58
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g86, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g13
  %tmp87 = load i32* %ind44
  %inc88 = add i32 %tmp87, 1
  store i32 %inc88, i32* %ind44
  br label %while45

merge91:                                          ; preds = %while45
  %i92 = load i32* %i
  %tmp93 = add i32 %i92, 1
  store i32 %tmp93, i32* %i
  br label %while

merge96:                                          ; preds = %while
  ret i32 0
}
