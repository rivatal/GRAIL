; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"display!1"({ { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %x) {
entry:
  %x1 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %x, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %x1
  ret void
}

define void @"display!2"({ { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %x) {
entry:
  %x1 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %x, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %x1
  ret void
}

define void @main() {
entry:
  %0 = alloca { i32 }
  %ptr = getelementptr inbounds { i32 }* %0, i32 0, i32 0
  store i32 1, i32* %ptr
  %1 = load { i32 }* %0
  %2 = alloca { i32 }
  %ptr1 = getelementptr inbounds { i32 }* %2, i32 0, i32 0
  store i32 2, i32* %ptr1
  %3 = load { i32 }* %2
  %4 = alloca { i32 }
  %ptr2 = getelementptr inbounds { i32 }* %4, i32 0, i32 0
  store i32 3, i32* %ptr2
  %5 = load { i32 }* %4
  %6 = alloca { i32 }
  %ptr3 = getelementptr inbounds { i32 }* %6, i32 0, i32 0
  store i32 4, i32* %ptr3
  %7 = load { i32 }* %6
  %8 = alloca { i32 }
  %ptr4 = getelementptr inbounds { i32 }* %8, i32 0, i32 0
  store i32 5, i32* %ptr4
  %9 = load { i32 }* %8
  %10 = alloca { i32 }
  %ptr5 = getelementptr inbounds { i32 }* %10, i32 0, i32 0
  store i32 6, i32* %ptr5
  %11 = load { i32 }* %10
  %12 = alloca { i32 }
  %ptr6 = getelementptr inbounds { i32 }* %12, i32 0, i32 0
  store i32 7, i32* %ptr6
  %13 = load { i32 }* %12
  %14 = alloca { i32 }
  %ptr7 = getelementptr inbounds { i32 }* %14, i32 0, i32 0
  store i32 8, i32* %ptr7
  %15 = load { i32 }* %14
  %16 = alloca { i32 }
  %ptr8 = getelementptr inbounds { i32 }* %16, i32 0, i32 0
  store i32 9, i32* %ptr8
  %17 = load { i32 }* %16
  %18 = alloca { i32 }
  %ptr9 = getelementptr inbounds { i32 }* %18, i32 0, i32 0
  store i32 10, i32* %ptr9
  %19 = load { i32 }* %18
  %strct = alloca { { i32 }*, i32 }
  %lst = alloca { i32 }, i32 10
  %ptr10 = getelementptr inbounds { i32 }* %lst, i32 0
  store { i32 } %1, { i32 }* %ptr10
  %ptr11 = getelementptr inbounds { i32 }* %lst, i32 1
  store { i32 } %3, { i32 }* %ptr11
  %ptr12 = getelementptr inbounds { i32 }* %lst, i32 2
  store { i32 } %5, { i32 }* %ptr12
  %ptr13 = getelementptr inbounds { i32 }* %lst, i32 3
  store { i32 } %7, { i32 }* %ptr13
  %ptr14 = getelementptr inbounds { i32 }* %lst, i32 4
  store { i32 } %9, { i32 }* %ptr14
  %ptr15 = getelementptr inbounds { i32 }* %lst, i32 5
  store { i32 } %11, { i32 }* %ptr15
  %ptr16 = getelementptr inbounds { i32 }* %lst, i32 6
  store { i32 } %13, { i32 }* %ptr16
  %ptr17 = getelementptr inbounds { i32 }* %lst, i32 7
  store { i32 } %15, { i32 }* %ptr17
  %ptr18 = getelementptr inbounds { i32 }* %lst, i32 8
  store { i32 } %17, { i32 }* %ptr18
  %ptr19 = getelementptr inbounds { i32 }* %lst, i32 9
  store { i32 } %19, { i32 }* %ptr19
  %p0 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 1
  store { i32 }* %lst, { i32 }** %p0
  store i32 10, i32* %p1
  %lst20 = load { { i32 }*, i32 }* %strct
  %petenodes = alloca { { i32 }*, i32 }
  store { { i32 }*, i32 } %lst20, { { i32 }*, i32 }* %petenodes
  %20 = alloca { i32 }
  %ptr21 = getelementptr inbounds { i32 }* %20, i32 0, i32 0
  store i32 1, i32* %ptr21
  %21 = load { i32 }* %20
  %g = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  %ptr22 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 2
  store { i32 } %21, { i32 }* %ptr22
  %22 = alloca { i32 }
  %ptr23 = getelementptr inbounds { i32 }* %22, i32 0, i32 0
  store i32 0, i32* %ptr23
  %23 = load { i32 }* %22
  %strct24 = alloca { { i32 }*, i32 }
  %lst25 = alloca { i32 }
  %ptr26 = getelementptr inbounds { i32 }* %lst25, i32 0
  store { i32 } %23, { i32 }* %ptr26
  %p027 = getelementptr inbounds { { i32 }*, i32 }* %strct24, i32 0, i32 0
  %p128 = getelementptr inbounds { { i32 }*, i32 }* %strct24, i32 0, i32 1
  store { i32 }* %lst25, { i32 }** %p027
  store i32 1, i32* %p128
  %lst29 = load { { i32 }*, i32 }* %strct24
  %strct30 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %lst31 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 0
  %p032 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct30, i32 0, i32 0
  %p133 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct30, i32 0, i32 1
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst31, { { i32 }*, { i32 }*, i1, { i32 } }** %p032
  store i32 0, i32* %p133
  %lst34 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct30
  %ptr35 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 0
  store { { i32 }*, i32 } %lst29, { { i32 }*, i32 }* %ptr35
  %ptr36 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %lst34, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr36
  %g37 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g
  %pete = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g37, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %lst38 = alloca { { i32 }*, i32 }
  %petenodes39 = load { { i32 }*, i32 }* %petenodes
  store { { i32 }*, i32 } %petenodes39, { { i32 }*, i32 }* %lst38
  %n = alloca { i32 }
  %tmp = getelementptr inbounds { { i32 }*, i32 }* %lst38, i32 0, i32 0
  %ar = load { i32 }** %tmp
  %tmp40 = getelementptr inbounds { { i32 }*, i32 }* %lst38, i32 0, i32 1
  %end = load i32* %tmp40
  %ind = alloca i32
  store i32 0, i32* %ind
  br label %while

while:                                            ; preds = %merge, %entry
  %tmp69 = load i32* %ind
  %comp70 = icmp slt i32 %tmp69, %end
  br i1 %comp70, label %while_body, label %merge71

while_body:                                       ; preds = %while
  %i = load i32* %ind
  %ptr41 = getelementptr inbounds { i32 }* %ar, i32 %i
  %tmp42 = load { i32 }* %ptr41
  store { i32 } %tmp42, { i32 }* %n
  %pete43 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %n44 = load { i32 }* %n
  %strct45 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete43, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct45
  %ptr46 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct45, i32 0, i32 0
  %nodes = load { { i32 }*, i32 }* %ptr46
  %strct47 = alloca { { i32 }*, i32 }
  %strct48 = alloca { { i32 }*, i32 }
  store { { i32 }*, i32 } %nodes, { { i32 }*, i32 }* %strct48
  %tmp49 = getelementptr inbounds { { i32 }*, i32 }* %strct48, i32 0, i32 1
  %len = load i32* %tmp49
  %tmp50 = getelementptr inbounds { { i32 }*, i32 }* %strct48, i32 0, i32 0
  %lst51 = load { i32 }** %tmp50
  %len52 = add i32 1, %len
  %tmp53 = getelementptr inbounds { { i32 }*, i32 }* %strct47, i32 0, i32 1
  store i32 %len52, i32* %tmp53
  %lst54 = alloca { i32 }, i32 %len52
  %ptr55 = getelementptr inbounds { i32 }* %lst54, i32 %len
  store { i32 } %n44, { i32 }* %ptr55
  %ind56 = alloca i32
  store i32 0, i32* %ind56
  br label %checklimits

checklimits:                                      ; preds = %assignment, %while_body
  %tmp62 = load i32* %ind56
  %comp = icmp slt i32 %tmp62, %len
  br i1 %comp, label %assignment, label %merge

assignment:                                       ; preds = %checklimits
  %i57 = load i32* %ind56
  %ptr58 = getelementptr inbounds { i32 }* %lst51, i32 %i57
  %ptr59 = getelementptr inbounds { i32 }* %lst54, i32 %i57
  %tmp60 = load { i32 }* %ptr58
  store { i32 } %tmp60, { i32 }* %ptr59
  %tmp61 = load i32* %ind56
  %inc = add i32 %tmp61, 1
  store i32 %inc, i32* %ind56
  br label %checklimits

merge:                                            ; preds = %checklimits
  %tmp63 = getelementptr inbounds { { i32 }*, i32 }* %strct47, i32 0, i32 0
  store { i32 }* %lst54, { i32 }** %tmp63
  %strct64 = load { { i32 }*, i32 }* %strct47
  %tmp65 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct45, i32 0, i32 0
  store { { i32 }*, i32 } %strct64, { { i32 }*, i32 }* %tmp65
  %g66 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct45
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g66, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %tmp67 = load i32* %ind
  %inc68 = add i32 %tmp67, 1
  store i32 %inc68, i32* %ind
  br label %while

merge71:                                          ; preds = %while
  %i72 = alloca i32
  store i32 0, i32* %i72
  br label %while73

while73:                                          ; preds = %merge296, %merge71
  %i343 = load i32* %i72
  %tmp344 = icmp slt i32 %i343, 5
  br i1 %tmp344, label %while_body74, label %merge345

while_body74:                                     ; preds = %while73
  %tmp75 = getelementptr inbounds { { i32 }*, i32 }* %petenodes, i32 0, i32 0
  %tmpar = load { i32 }** %tmp75
  %i76 = load i32* %i72
  %ptr77 = getelementptr inbounds { i32 }* %tmpar, i32 %i76
  %item = load { i32 }* %ptr77
  %pi = alloca { i32 }
  store { i32 } %item, { i32 }* %pi
  %tmp78 = getelementptr inbounds { { i32 }*, i32 }* %petenodes, i32 0, i32 0
  %tmpar79 = load { i32 }** %tmp78
  %i80 = load i32* %i72
  %tmp81 = add i32 %i80, 5
  %ptr82 = getelementptr inbounds { i32 }* %tmpar79, i32 %tmp81
  %item83 = load { i32 }* %ptr82
  %po = alloca { i32 }
  store { i32 } %item83, { i32 }* %po
  %pete84 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %g85 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete84, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g85
  %ptr86 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g85, i32 0, i32 2
  %tmp87 = load { i32 }* %ptr86
  %24 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr88 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %24, i32 0, i32 0
  store { i32 }* %pi, { i32 }** %ptr88
  %ptr89 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %24, i32 0, i32 1
  store { i32 }* %po, { i32 }** %ptr89
  %ptr90 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %24, i32 0, i32 2
  store i1 false, i1* %ptr90
  %ptr91 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %24, i32 0, i32 3
  store { i32 } %tmp87, { i32 }* %ptr91
  %25 = load { { i32 }*, { i32 }*, i1, { i32 } }* %24
  %strct92 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete84, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct92
  %ptr93 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct92, i32 0, i32 1
  %nodes94 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr93
  %strct95 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %strct96 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %nodes94, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct96
  %tmp97 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct96, i32 0, i32 1
  %len98 = load i32* %tmp97
  %tmp99 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct96, i32 0, i32 0
  %lst100 = load { { i32 }*, { i32 }*, i1, { i32 } }** %tmp99
  %len101 = add i32 1, %len98
  %tmp102 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct95, i32 0, i32 1
  store i32 %len101, i32* %tmp102
  %lst103 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 %len101
  %ptr104 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst103, i32 %len98
  store { { i32 }*, { i32 }*, i1, { i32 } } %25, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr104
  %ind105 = alloca i32
  store i32 0, i32* %ind105
  br label %checklimits106

checklimits106:                                   ; preds = %assignment107, %while_body74
  %tmp114 = load i32* %ind105
  %comp115 = icmp slt i32 %tmp114, %len98
  br i1 %comp115, label %assignment107, label %merge116

assignment107:                                    ; preds = %checklimits106
  %i108 = load i32* %ind105
  %ptr109 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst100, i32 %i108
  %ptr110 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst103, i32 %i108
  %tmp111 = load { { i32 }*, { i32 }*, i1, { i32 } }* %ptr109
  store { { i32 }*, { i32 }*, i1, { i32 } } %tmp111, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr110
  %tmp112 = load i32* %ind105
  %inc113 = add i32 %tmp112, 1
  store i32 %inc113, i32* %ind105
  br label %checklimits106

merge116:                                         ; preds = %checklimits106
  %tmp117 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct95, i32 0, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst103, { { i32 }*, { i32 }*, i1, { i32 } }** %tmp117
  %strct118 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct95
  %tmp119 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct92, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %strct118, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %tmp119
  %g120 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct92
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g120, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %i121 = load i32* %i72
  %tmp122 = icmp eq i32 %i121, 0
  br i1 %tmp122, label %then, label %else

merge123:                                         ; preds = %else, %merge201
  %i206 = load i32* %i72
  %tmp207 = icmp eq i32 %i206, 1
  br i1 %tmp207, label %then209, label %else293

then:                                             ; preds = %merge116
  %tmp124 = getelementptr inbounds { { i32 }*, i32 }* %petenodes, i32 0, i32 0
  %tmpar125 = load { i32 }** %tmp124
  %ptr126 = getelementptr inbounds { i32 }* %tmpar125, i32 2
  %item127 = load { i32 }* %ptr126
  %p2 = alloca { i32 }
  store { i32 } %item127, { i32 }* %p2
  %tmp128 = getelementptr inbounds { { i32 }*, i32 }* %petenodes, i32 0, i32 0
  %tmpar129 = load { i32 }** %tmp128
  %ptr130 = getelementptr inbounds { i32 }* %tmpar129, i32 3
  %item131 = load { i32 }* %ptr130
  %p3 = alloca { i32 }
  store { i32 } %item131, { i32 }* %p3
  %pete132 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %g133 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete132, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g133
  %ptr134 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g133, i32 0, i32 2
  %tmp135 = load { i32 }* %ptr134
  %26 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr136 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %26, i32 0, i32 0
  store { i32 }* %pi, { i32 }** %ptr136
  %ptr137 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %26, i32 0, i32 1
  store { i32 }* %p2, { i32 }** %ptr137
  %ptr138 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %26, i32 0, i32 2
  store i1 false, i1* %ptr138
  %ptr139 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %26, i32 0, i32 3
  store { i32 } %tmp135, { i32 }* %ptr139
  %27 = load { { i32 }*, { i32 }*, i1, { i32 } }* %26
  %strct140 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete132, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct140
  %ptr141 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct140, i32 0, i32 1
  %nodes142 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr141
  %strct143 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %strct144 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %nodes142, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct144
  %tmp145 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct144, i32 0, i32 1
  %len146 = load i32* %tmp145
  %tmp147 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct144, i32 0, i32 0
  %lst148 = load { { i32 }*, { i32 }*, i1, { i32 } }** %tmp147
  %len149 = add i32 1, %len146
  %tmp150 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct143, i32 0, i32 1
  store i32 %len149, i32* %tmp150
  %lst151 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 %len149
  %ptr152 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst151, i32 %len146
  store { { i32 }*, { i32 }*, i1, { i32 } } %27, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr152
  %ind153 = alloca i32
  store i32 0, i32* %ind153
  br label %checklimits154

checklimits154:                                   ; preds = %assignment155, %then
  %tmp162 = load i32* %ind153
  %comp163 = icmp slt i32 %tmp162, %len146
  br i1 %comp163, label %assignment155, label %merge164

assignment155:                                    ; preds = %checklimits154
  %i156 = load i32* %ind153
  %ptr157 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst148, i32 %i156
  %ptr158 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst151, i32 %i156
  %tmp159 = load { { i32 }*, { i32 }*, i1, { i32 } }* %ptr157
  store { { i32 }*, { i32 }*, i1, { i32 } } %tmp159, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr158
  %tmp160 = load i32* %ind153
  %inc161 = add i32 %tmp160, 1
  store i32 %inc161, i32* %ind153
  br label %checklimits154

merge164:                                         ; preds = %checklimits154
  %tmp165 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct143, i32 0, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst151, { { i32 }*, { i32 }*, i1, { i32 } }** %tmp165
  %strct166 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct143
  %tmp167 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct140, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %strct166, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %tmp167
  %g168 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct140
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g168, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %pete169 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %g170 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete169, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g170
  %ptr171 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g170, i32 0, i32 2
  %tmp172 = load { i32 }* %ptr171
  %28 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr173 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %28, i32 0, i32 0
  store { i32 }* %pi, { i32 }** %ptr173
  %ptr174 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %28, i32 0, i32 1
  store { i32 }* %p3, { i32 }** %ptr174
  %ptr175 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %28, i32 0, i32 2
  store i1 false, i1* %ptr175
  %ptr176 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %28, i32 0, i32 3
  store { i32 } %tmp172, { i32 }* %ptr176
  %29 = load { { i32 }*, { i32 }*, i1, { i32 } }* %28
  %strct177 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete169, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct177
  %ptr178 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct177, i32 0, i32 1
  %nodes179 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr178
  %strct180 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %strct181 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %nodes179, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct181
  %tmp182 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct181, i32 0, i32 1
  %len183 = load i32* %tmp182
  %tmp184 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct181, i32 0, i32 0
  %lst185 = load { { i32 }*, { i32 }*, i1, { i32 } }** %tmp184
  %len186 = add i32 1, %len183
  %tmp187 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct180, i32 0, i32 1
  store i32 %len186, i32* %tmp187
  %lst188 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 %len186
  %ptr189 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst188, i32 %len183
  store { { i32 }*, { i32 }*, i1, { i32 } } %29, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr189
  %ind190 = alloca i32
  store i32 0, i32* %ind190
  br label %checklimits191

checklimits191:                                   ; preds = %assignment192, %merge164
  %tmp199 = load i32* %ind190
  %comp200 = icmp slt i32 %tmp199, %len183
  br i1 %comp200, label %assignment192, label %merge201

assignment192:                                    ; preds = %checklimits191
  %i193 = load i32* %ind190
  %ptr194 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst185, i32 %i193
  %ptr195 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst188, i32 %i193
  %tmp196 = load { { i32 }*, { i32 }*, i1, { i32 } }* %ptr194
  store { { i32 }*, { i32 }*, i1, { i32 } } %tmp196, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr195
  %tmp197 = load i32* %ind190
  %inc198 = add i32 %tmp197, 1
  store i32 %inc198, i32* %ind190
  br label %checklimits191

merge201:                                         ; preds = %checklimits191
  %tmp202 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct180, i32 0, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst188, { { i32 }*, { i32 }*, i1, { i32 } }** %tmp202
  %strct203 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct180
  %tmp204 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct177, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %strct203, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %tmp204
  %g205 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct177
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g205, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  br label %merge123

else:                                             ; preds = %merge116
  br label %merge123

merge208:                                         ; preds = %else293, %merge288
  %i294 = load i32* %i72
  %tmp295 = icmp eq i32 %i294, 2
  br i1 %tmp295, label %then297, label %else340

then209:                                          ; preds = %merge123
  %tmp210 = getelementptr inbounds { { i32 }*, i32 }* %petenodes, i32 0, i32 0
  %tmpar211 = load { i32 }** %tmp210
  %ptr212 = getelementptr inbounds { i32 }* %tmpar211, i32 3
  %item213 = load { i32 }* %ptr212
  %p3214 = alloca { i32 }
  store { i32 } %item213, { i32 }* %p3214
  %tmp215 = getelementptr inbounds { { i32 }*, i32 }* %petenodes, i32 0, i32 0
  %tmpar216 = load { i32 }** %tmp215
  %ptr217 = getelementptr inbounds { i32 }* %tmpar216, i32 4
  %item218 = load { i32 }* %ptr217
  %p4 = alloca { i32 }
  store { i32 } %item218, { i32 }* %p4
  %pete219 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %g220 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete219, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g220
  %ptr221 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g220, i32 0, i32 2
  %tmp222 = load { i32 }* %ptr221
  %30 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr223 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %30, i32 0, i32 0
  store { i32 }* %pi, { i32 }** %ptr223
  %ptr224 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %30, i32 0, i32 1
  store { i32 }* %p3214, { i32 }** %ptr224
  %ptr225 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %30, i32 0, i32 2
  store i1 false, i1* %ptr225
  %ptr226 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %30, i32 0, i32 3
  store { i32 } %tmp222, { i32 }* %ptr226
  %31 = load { { i32 }*, { i32 }*, i1, { i32 } }* %30
  %strct227 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete219, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct227
  %ptr228 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct227, i32 0, i32 1
  %nodes229 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr228
  %strct230 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %strct231 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %nodes229, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct231
  %tmp232 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct231, i32 0, i32 1
  %len233 = load i32* %tmp232
  %tmp234 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct231, i32 0, i32 0
  %lst235 = load { { i32 }*, { i32 }*, i1, { i32 } }** %tmp234
  %len236 = add i32 1, %len233
  %tmp237 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct230, i32 0, i32 1
  store i32 %len236, i32* %tmp237
  %lst238 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 %len236
  %ptr239 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst238, i32 %len233
  store { { i32 }*, { i32 }*, i1, { i32 } } %31, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr239
  %ind240 = alloca i32
  store i32 0, i32* %ind240
  br label %checklimits241

checklimits241:                                   ; preds = %assignment242, %then209
  %tmp249 = load i32* %ind240
  %comp250 = icmp slt i32 %tmp249, %len233
  br i1 %comp250, label %assignment242, label %merge251

assignment242:                                    ; preds = %checklimits241
  %i243 = load i32* %ind240
  %ptr244 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst235, i32 %i243
  %ptr245 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst238, i32 %i243
  %tmp246 = load { { i32 }*, { i32 }*, i1, { i32 } }* %ptr244
  store { { i32 }*, { i32 }*, i1, { i32 } } %tmp246, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr245
  %tmp247 = load i32* %ind240
  %inc248 = add i32 %tmp247, 1
  store i32 %inc248, i32* %ind240
  br label %checklimits241

merge251:                                         ; preds = %checklimits241
  %tmp252 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct230, i32 0, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst238, { { i32 }*, { i32 }*, i1, { i32 } }** %tmp252
  %strct253 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct230
  %tmp254 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct227, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %strct253, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %tmp254
  %g255 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct227
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g255, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %pete256 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %g257 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete256, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g257
  %ptr258 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g257, i32 0, i32 2
  %tmp259 = load { i32 }* %ptr258
  %32 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr260 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %32, i32 0, i32 0
  store { i32 }* %pi, { i32 }** %ptr260
  %ptr261 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %32, i32 0, i32 1
  store { i32 }* %p4, { i32 }** %ptr261
  %ptr262 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %32, i32 0, i32 2
  store i1 false, i1* %ptr262
  %ptr263 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %32, i32 0, i32 3
  store { i32 } %tmp259, { i32 }* %ptr263
  %33 = load { { i32 }*, { i32 }*, i1, { i32 } }* %32
  %strct264 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete256, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct264
  %ptr265 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct264, i32 0, i32 1
  %nodes266 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr265
  %strct267 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %strct268 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %nodes266, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct268
  %tmp269 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct268, i32 0, i32 1
  %len270 = load i32* %tmp269
  %tmp271 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct268, i32 0, i32 0
  %lst272 = load { { i32 }*, { i32 }*, i1, { i32 } }** %tmp271
  %len273 = add i32 1, %len270
  %tmp274 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct267, i32 0, i32 1
  store i32 %len273, i32* %tmp274
  %lst275 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 %len273
  %ptr276 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst275, i32 %len270
  store { { i32 }*, { i32 }*, i1, { i32 } } %33, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr276
  %ind277 = alloca i32
  store i32 0, i32* %ind277
  br label %checklimits278

checklimits278:                                   ; preds = %assignment279, %merge251
  %tmp286 = load i32* %ind277
  %comp287 = icmp slt i32 %tmp286, %len270
  br i1 %comp287, label %assignment279, label %merge288

assignment279:                                    ; preds = %checklimits278
  %i280 = load i32* %ind277
  %ptr281 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst272, i32 %i280
  %ptr282 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst275, i32 %i280
  %tmp283 = load { { i32 }*, { i32 }*, i1, { i32 } }* %ptr281
  store { { i32 }*, { i32 }*, i1, { i32 } } %tmp283, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr282
  %tmp284 = load i32* %ind277
  %inc285 = add i32 %tmp284, 1
  store i32 %inc285, i32* %ind277
  br label %checklimits278

merge288:                                         ; preds = %checklimits278
  %tmp289 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct267, i32 0, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst275, { { i32 }*, { i32 }*, i1, { i32 } }** %tmp289
  %strct290 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct267
  %tmp291 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct264, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %strct290, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %tmp291
  %g292 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct264
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g292, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  br label %merge208

else293:                                          ; preds = %merge123
  br label %merge208

merge296:                                         ; preds = %else340, %merge335
  %i341 = load i32* %i72
  %tmp342 = add i32 %i341, 1
  store i32 %tmp342, i32* %i72
  br label %while73

then297:                                          ; preds = %merge208
  %tmp298 = getelementptr inbounds { { i32 }*, i32 }* %petenodes, i32 0, i32 0
  %tmpar299 = load { i32 }** %tmp298
  %ptr300 = getelementptr inbounds { i32 }* %tmpar299, i32 4
  %item301 = load { i32 }* %ptr300
  %p4302 = alloca { i32 }
  store { i32 } %item301, { i32 }* %p4302
  %pete303 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %g304 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete303, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g304
  %ptr305 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g304, i32 0, i32 2
  %tmp306 = load { i32 }* %ptr305
  %34 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr307 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %34, i32 0, i32 0
  store { i32 }* %pi, { i32 }** %ptr307
  %ptr308 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %34, i32 0, i32 1
  store { i32 }* %p4302, { i32 }** %ptr308
  %ptr309 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %34, i32 0, i32 2
  store i1 false, i1* %ptr309
  %ptr310 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %34, i32 0, i32 3
  store { i32 } %tmp306, { i32 }* %ptr310
  %35 = load { { i32 }*, { i32 }*, i1, { i32 } }* %34
  %strct311 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete303, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct311
  %ptr312 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct311, i32 0, i32 1
  %nodes313 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr312
  %strct314 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %strct315 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %nodes313, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct315
  %tmp316 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct315, i32 0, i32 1
  %len317 = load i32* %tmp316
  %tmp318 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct315, i32 0, i32 0
  %lst319 = load { { i32 }*, { i32 }*, i1, { i32 } }** %tmp318
  %len320 = add i32 1, %len317
  %tmp321 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct314, i32 0, i32 1
  store i32 %len320, i32* %tmp321
  %lst322 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 %len320
  %ptr323 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst322, i32 %len317
  store { { i32 }*, { i32 }*, i1, { i32 } } %35, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr323
  %ind324 = alloca i32
  store i32 0, i32* %ind324
  br label %checklimits325

checklimits325:                                   ; preds = %assignment326, %then297
  %tmp333 = load i32* %ind324
  %comp334 = icmp slt i32 %tmp333, %len317
  br i1 %comp334, label %assignment326, label %merge335

assignment326:                                    ; preds = %checklimits325
  %i327 = load i32* %ind324
  %ptr328 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst319, i32 %i327
  %ptr329 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst322, i32 %i327
  %tmp330 = load { { i32 }*, { i32 }*, i1, { i32 } }* %ptr328
  store { { i32 }*, { i32 }*, i1, { i32 } } %tmp330, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr329
  %tmp331 = load i32* %ind324
  %inc332 = add i32 %tmp331, 1
  store i32 %inc332, i32* %ind324
  br label %checklimits325

merge335:                                         ; preds = %checklimits325
  %tmp336 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct314, i32 0, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst322, { { i32 }*, { i32 }*, i1, { i32 } }** %tmp336
  %strct337 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct314
  %tmp338 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct311, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %strct337, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %tmp338
  %g339 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct311
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g339, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  br label %merge296

else340:                                          ; preds = %merge208
  br label %merge296

merge345:                                         ; preds = %while73
  store i32 5, i32* %i72
  br label %while346

while346:                                         ; preds = %merge399, %merge345
  %i445 = load i32* %i72
  %tmp446 = icmp slt i32 %i445, 9
  br i1 %tmp446, label %while_body347, label %merge447

while_body347:                                    ; preds = %while346
  %tmp348 = getelementptr inbounds { { i32 }*, i32 }* %petenodes, i32 0, i32 0
  %tmpar349 = load { i32 }** %tmp348
  %i350 = load i32* %i72
  %ptr351 = getelementptr inbounds { i32 }* %tmpar349, i32 %i350
  %item352 = load { i32 }* %ptr351
  %pi353 = alloca { i32 }
  store { i32 } %item352, { i32 }* %pi353
  %tmp354 = getelementptr inbounds { { i32 }*, i32 }* %petenodes, i32 0, i32 0
  %tmpar355 = load { i32 }** %tmp354
  %i356 = load i32* %i72
  %tmp357 = add i32 %i356, 1
  %ptr358 = getelementptr inbounds { i32 }* %tmpar355, i32 %tmp357
  %item359 = load { i32 }* %ptr358
  %pplus = alloca { i32 }
  store { i32 } %item359, { i32 }* %pplus
  %pete360 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %g361 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete360, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g361
  %ptr362 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g361, i32 0, i32 2
  %tmp363 = load { i32 }* %ptr362
  %36 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr364 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %36, i32 0, i32 0
  store { i32 }* %pi353, { i32 }** %ptr364
  %ptr365 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %36, i32 0, i32 1
  store { i32 }* %pplus, { i32 }** %ptr365
  %ptr366 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %36, i32 0, i32 2
  store i1 false, i1* %ptr366
  %ptr367 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %36, i32 0, i32 3
  store { i32 } %tmp363, { i32 }* %ptr367
  %37 = load { { i32 }*, { i32 }*, i1, { i32 } }* %36
  %strct368 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete360, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct368
  %ptr369 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct368, i32 0, i32 1
  %nodes370 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr369
  %strct371 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %strct372 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %nodes370, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct372
  %tmp373 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct372, i32 0, i32 1
  %len374 = load i32* %tmp373
  %tmp375 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct372, i32 0, i32 0
  %lst376 = load { { i32 }*, { i32 }*, i1, { i32 } }** %tmp375
  %len377 = add i32 1, %len374
  %tmp378 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct371, i32 0, i32 1
  store i32 %len377, i32* %tmp378
  %lst379 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 %len377
  %ptr380 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst379, i32 %len374
  store { { i32 }*, { i32 }*, i1, { i32 } } %37, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr380
  %ind381 = alloca i32
  store i32 0, i32* %ind381
  br label %checklimits382

checklimits382:                                   ; preds = %assignment383, %while_body347
  %tmp390 = load i32* %ind381
  %comp391 = icmp slt i32 %tmp390, %len374
  br i1 %comp391, label %assignment383, label %merge392

assignment383:                                    ; preds = %checklimits382
  %i384 = load i32* %ind381
  %ptr385 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst376, i32 %i384
  %ptr386 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst379, i32 %i384
  %tmp387 = load { { i32 }*, { i32 }*, i1, { i32 } }* %ptr385
  store { { i32 }*, { i32 }*, i1, { i32 } } %tmp387, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr386
  %tmp388 = load i32* %ind381
  %inc389 = add i32 %tmp388, 1
  store i32 %inc389, i32* %ind381
  br label %checklimits382

merge392:                                         ; preds = %checklimits382
  %tmp393 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct371, i32 0, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst379, { { i32 }*, { i32 }*, i1, { i32 } }** %tmp393
  %strct394 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct371
  %tmp395 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct368, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %strct394, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %tmp395
  %g396 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct368
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g396, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %i397 = load i32* %i72
  %tmp398 = icmp eq i32 %i397, 5
  br i1 %tmp398, label %then400, label %else442

merge399:                                         ; preds = %else442, %merge437
  %i443 = load i32* %i72
  %tmp444 = add i32 %i443, 1
  store i32 %tmp444, i32* %i72
  br label %while346

then400:                                          ; preds = %merge392
  %tmp401 = getelementptr inbounds { { i32 }*, i32 }* %petenodes, i32 0, i32 0
  %tmpar402 = load { i32 }** %tmp401
  %ptr403 = getelementptr inbounds { i32 }* %tmpar402, i32 9
  %item404 = load { i32 }* %ptr403
  %p9 = alloca { i32 }
  store { i32 } %item404, { i32 }* %p9
  %pete405 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %g406 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete405, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g406
  %ptr407 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g406, i32 0, i32 2
  %tmp408 = load { i32 }* %ptr407
  %38 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr409 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %38, i32 0, i32 0
  store { i32 }* %pi353, { i32 }** %ptr409
  %ptr410 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %38, i32 0, i32 1
  store { i32 }* %p9, { i32 }** %ptr410
  %ptr411 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %38, i32 0, i32 2
  store i1 false, i1* %ptr411
  %ptr412 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %38, i32 0, i32 3
  store { i32 } %tmp408, { i32 }* %ptr412
  %39 = load { { i32 }*, { i32 }*, i1, { i32 } }* %38
  %strct413 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete405, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct413
  %ptr414 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct413, i32 0, i32 1
  %nodes415 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr414
  %strct416 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %strct417 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %nodes415, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct417
  %tmp418 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct417, i32 0, i32 1
  %len419 = load i32* %tmp418
  %tmp420 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct417, i32 0, i32 0
  %lst421 = load { { i32 }*, { i32 }*, i1, { i32 } }** %tmp420
  %len422 = add i32 1, %len419
  %tmp423 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct416, i32 0, i32 1
  store i32 %len422, i32* %tmp423
  %lst424 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 %len422
  %ptr425 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst424, i32 %len419
  store { { i32 }*, { i32 }*, i1, { i32 } } %39, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr425
  %ind426 = alloca i32
  store i32 0, i32* %ind426
  br label %checklimits427

checklimits427:                                   ; preds = %assignment428, %then400
  %tmp435 = load i32* %ind426
  %comp436 = icmp slt i32 %tmp435, %len419
  br i1 %comp436, label %assignment428, label %merge437

assignment428:                                    ; preds = %checklimits427
  %i429 = load i32* %ind426
  %ptr430 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst421, i32 %i429
  %ptr431 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst424, i32 %i429
  %tmp432 = load { { i32 }*, { i32 }*, i1, { i32 } }* %ptr430
  store { { i32 }*, { i32 }*, i1, { i32 } } %tmp432, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr431
  %tmp433 = load i32* %ind426
  %inc434 = add i32 %tmp433, 1
  store i32 %inc434, i32* %ind426
  br label %checklimits427

merge437:                                         ; preds = %checklimits427
  %tmp438 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct416, i32 0, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst424, { { i32 }*, { i32 }*, i1, { i32 } }** %tmp438
  %strct439 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct416
  %tmp440 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct413, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %strct439, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %tmp440
  %g441 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %strct413
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g441, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  br label %merge399

else442:                                          ; preds = %merge392
  br label %merge399

merge447:                                         ; preds = %while346
  %pete448 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %pete
  %graph_display = call i32 @display({ { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %pete448)
  ret void
}

declare i32 @display({ { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } })
