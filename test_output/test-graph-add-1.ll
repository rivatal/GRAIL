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
  %a = alloca { i32 }
  store { i32 } %1, { i32 }* %a
  %2 = alloca { i32 }
  %ptr1 = getelementptr inbounds { i32 }* %2, i32 0, i32 0
  store i32 2, i32* %ptr1
  %3 = load { i32 }* %2
  %b = alloca { i32 }
  store { i32 } %3, { i32 }* %b
  %4 = alloca { i32 }
  %ptr2 = getelementptr inbounds { i32 }* %4, i32 0, i32 0
  store i32 3, i32* %ptr2
  %5 = load { i32 }* %4
  %c = alloca { i32 }
  store { i32 } %5, { i32 }* %c
  %6 = alloca { i32 }
  %ptr3 = getelementptr inbounds { i32 }* %6, i32 0, i32 0
  store i32 1, i32* %ptr3
  %7 = load { i32 }* %6
  %g = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  %ptr4 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 2
  store { i32 } %7, { i32 }* %ptr4
  %b5 = load { i32 }* %b
  %a6 = load { i32 }* %a
  %strct = alloca { { i32 }*, i32 }
  %lst = alloca { i32 }, i32 2
  %ptr7 = getelementptr inbounds { i32 }* %lst, i32 0
  store { i32 } %b5, { i32 }* %ptr7
  %ptr8 = getelementptr inbounds { i32 }* %lst, i32 1
  store { i32 } %a6, { i32 }* %ptr8
  %p0 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 1
  store { i32 }* %lst, { i32 }** %p0
  store i32 2, i32* %p1
  %lst9 = load { { i32 }*, i32 }* %strct
  %strct10 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %lst11 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 0
  %p012 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct10, i32 0, i32 0
  %p113 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct10, i32 0, i32 1
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst11, { { i32 }*, { i32 }*, i1, { i32 } }** %p012
  store i32 0, i32* %p113
  %lst14 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct10
  %ptr15 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 0
  store { { i32 }*, i32 } %lst9, { { i32 }*, i32 }* %ptr15
  %ptr16 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %lst14, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr16
  %g17 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g
  %g18 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g17, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g18
  %g19 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g18
  %c20 = load { i32 }* %c
  %strct21 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g19, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct21
  %ptr22 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct21, i32 0, i32 0
  %nodes = load { { i32 }*, i32 }* %ptr22
  %strct23 = alloca { { i32 }*, i32 }
  %strct24 = alloca { { i32 }*, i32 }
  store { { i32 }*, i32 } %nodes, { { i32 }*, i32 }* %strct24
  %tmp = getelementptr inbounds { { i32 }*, i32 }* %strct24, i32 0, i32 1
  %len = load i32* %tmp
  %tmp25 = getelementptr inbounds { { i32 }*, i32 }* %strct24, i32 0, i32 0
  %lst26 = load { i32 }** %tmp25
  %len27 = add i32 1, %len
  %tmp28 = getelementptr inbounds { { i32 }*, i32 }* %strct23, i32 0, i32 1
  store i32 %len27, i32* %tmp28
  %lst29 = alloca { i32 }, i32 %len27
  %ptr30 = getelementptr inbounds { i32 }* %lst29, i32 %len
  store { i32 } %c20, { i32 }* %ptr30
  %ind = alloca i32
  store i32 0, i32* %ind
  br label %checklimits

checklimits:                                      ; preds = %assignment, %entry
  %tmp35 = load i32* %ind
  %comp = icmp slt i32 %tmp35, %len
  br i1 %comp, label %assignment, label %merge

assignment:                                       ; preds = %checklimits
  %i = load i32* %ind
  %ptr31 = getelementptr inbounds { i32 }* %lst26, i32 %i
  %ptr32 = getelementptr inbounds { i32 }* %lst29, i32 %i
  %tmp33 = load { i32 }* %ptr31
  store { i32 } %tmp33, { i32 }* %ptr32
  %tmp34 = load i32* %ind
  %inc = add i32 %tmp34, 1
  store i32 %inc, i32* %ind
  br label %checklimits

merge:                                            ; preds = %checklimits
  %tmp36 = getelementptr inbounds { { i32 }*, i32 }* %strct23, i32 0, i32 0
  store { i32 }* %lst29, { i32 }** %tmp36
  %strct37 = load { { i32 }*, i32 }* %strct23
  %tmp38 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct21, i32 0, i32 0
  store { { i32 }*, i32 } %strct37, { { i32 }*, i32 }* %tmp38
  %g39 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct21
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g39, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g18
  %g40 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g18
  %g41 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g40, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g41
  %ptr42 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g41, i32 0, i32 2
  %tmp43 = load { i32 }* %ptr42
  %8 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr44 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %8, i32 0, i32 0
  store { i32 }* %a, { i32 }** %ptr44
  %ptr45 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %8, i32 0, i32 1
  store { i32 }* %c, { i32 }** %ptr45
  %ptr46 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %8, i32 0, i32 2
  store i1 true, i1* %ptr46
  %ptr47 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %8, i32 0, i32 3
  store { i32 } %tmp43, { i32 }* %ptr47
  %9 = load { { i32 }*, { i32 }*, i1, { i32 } }* %8
  %strct48 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g40, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct48
  %ptr49 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct48, i32 0, i32 1
  %nodes50 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr49
  %strct51 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %strct52 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %nodes50, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct52
  %tmp53 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct52, i32 0, i32 1
  %len54 = load i32* %tmp53
  %tmp55 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct52, i32 0, i32 0
  %lst56 = load { { i32 }*, { i32 }*, i1, { i32 } }** %tmp55
  %len57 = add i32 1, %len54
  %tmp58 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct51, i32 0, i32 1
  store i32 %len57, i32* %tmp58
  %lst59 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 %len57
  %ptr60 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst59, i32 %len54
  store { { i32 }*, { i32 }*, i1, { i32 } } %9, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr60
  %ind61 = alloca i32
  store i32 0, i32* %ind61
  br label %checklimits62

checklimits62:                                    ; preds = %assignment63, %merge
  %tmp70 = load i32* %ind61
  %comp71 = icmp slt i32 %tmp70, %len54
  br i1 %comp71, label %assignment63, label %merge72

assignment63:                                     ; preds = %checklimits62
  %i64 = load i32* %ind61
  %ptr65 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst56, i32 %i64
  %ptr66 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst59, i32 %i64
  %tmp67 = load { { i32 }*, { i32 }*, i1, { i32 } }* %ptr65
  store { { i32 }*, { i32 }*, i1, { i32 } } %tmp67, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr66
  %tmp68 = load i32* %ind61
  %inc69 = add i32 %tmp68, 1
  store i32 %inc69, i32* %ind61
  br label %checklimits62

merge72:                                          ; preds = %checklimits62
  %tmp73 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct51, i32 0, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst59, { { i32 }*, { i32 }*, i1, { i32 } }** %tmp73
  %strct74 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct51
  %tmp75 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct48, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %strct74, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %tmp75
  %g76 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct48
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g76, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g18
  ret i32 0
}
