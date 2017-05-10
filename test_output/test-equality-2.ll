; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt6 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt7 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt8 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt9 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt10 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt11 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt12 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt13 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt14 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt15 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt16 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt17 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"printbool!1"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printbool!2"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printbool!3"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printbool!4"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printbool!5"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printbool!6"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printbool!7"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define void @"printbool!8"(i1 %x) {
entry:
  %x1 = alloca i1
  store i1 %x, i1* %x1
  ret void
}

define i32 @main() {
entry:
  %0 = alloca { i32, i32 }
  %ptr = getelementptr inbounds { i32, i32 }* %0, i32 0, i32 0
  store i32 1, i32* %ptr
  %ptr1 = getelementptr inbounds { i32, i32 }* %0, i32 0, i32 1
  store i32 2, i32* %ptr1
  %1 = load { i32, i32 }* %0
  %x = alloca { i32, i32 }
  store { i32, i32 } %1, { i32, i32 }* %x
  %2 = alloca { i32, i32 }
  %ptr2 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 0
  store i32 1, i32* %ptr2
  %ptr3 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 1
  store i32 2, i32* %ptr3
  %3 = load { i32, i32 }* %2
  %y = alloca { i32, i32 }
  store { i32, i32 } %3, { i32, i32 }* %y
  %4 = alloca { i32, i32 }
  %ptr4 = getelementptr inbounds { i32, i32 }* %4, i32 0, i32 0
  store i32 1, i32* %ptr4
  %ptr5 = getelementptr inbounds { i32, i32 }* %4, i32 0, i32 1
  store i32 3, i32* %ptr5
  %5 = load { i32, i32 }* %4
  %z = alloca { i32, i32 }
  store { i32, i32 } %5, { i32, i32 }* %z
  %6 = alloca { i32 }
  %ptr6 = getelementptr inbounds { i32 }* %6, i32 0, i32 0
  store i32 1, i32* %ptr6
  %7 = load { i32 }* %6
  %8 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr7 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %8, i32 0, i32 0
  store { i32, i32 }* %x, { i32, i32 }** %ptr7
  %ptr8 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %8, i32 0, i32 1
  store { i32, i32 }* %y, { i32, i32 }** %ptr8
  %ptr9 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %8, i32 0, i32 2
  store i1 true, i1* %ptr9
  %ptr10 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %8, i32 0, i32 3
  store { i32 } %7, { i32 }* %ptr10
  %9 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %8
  %e1 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %9, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e1
  %10 = alloca { i32 }
  %ptr11 = getelementptr inbounds { i32 }* %10, i32 0, i32 0
  store i32 1, i32* %ptr11
  %11 = load { i32 }* %10
  %12 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr12 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %12, i32 0, i32 0
  store { i32, i32 }* %x, { i32, i32 }** %ptr12
  %ptr13 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %12, i32 0, i32 1
  store { i32, i32 }* %x, { i32, i32 }** %ptr13
  %ptr14 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %12, i32 0, i32 2
  store i1 true, i1* %ptr14
  %ptr15 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %12, i32 0, i32 3
  store { i32 } %11, { i32 }* %ptr15
  %13 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %12
  %e2 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %13, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e2
  %14 = alloca { i32 }
  %ptr16 = getelementptr inbounds { i32 }* %14, i32 0, i32 0
  store i32 1, i32* %ptr16
  %15 = load { i32 }* %14
  %16 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr17 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %16, i32 0, i32 0
  store { i32, i32 }* %x, { i32, i32 }** %ptr17
  %ptr18 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %16, i32 0, i32 1
  store { i32, i32 }* %z, { i32, i32 }** %ptr18
  %ptr19 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %16, i32 0, i32 2
  store i1 true, i1* %ptr19
  %ptr20 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %16, i32 0, i32 3
  store { i32 } %15, { i32 }* %ptr20
  %17 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %16
  %e3 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %17, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e3
  %18 = alloca { i32 }
  %ptr21 = getelementptr inbounds { i32 }* %18, i32 0, i32 0
  store i32 2, i32* %ptr21
  %19 = load { i32 }* %18
  %20 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr22 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %20, i32 0, i32 0
  store { i32, i32 }* %x, { i32, i32 }** %ptr22
  %ptr23 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %20, i32 0, i32 1
  store { i32, i32 }* %y, { i32, i32 }** %ptr23
  %ptr24 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %20, i32 0, i32 2
  store i1 true, i1* %ptr24
  %ptr25 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %20, i32 0, i32 3
  store { i32 } %19, { i32 }* %ptr25
  %21 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %20
  %e4 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %21, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e4
  %22 = alloca { i32 }
  %ptr26 = getelementptr inbounds { i32 }* %22, i32 0, i32 0
  store i32 1, i32* %ptr26
  %23 = load { i32 }* %22
  %24 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr27 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %24, i32 0, i32 0
  store { i32, i32 }* %x, { i32, i32 }** %ptr27
  %ptr28 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %24, i32 0, i32 1
  store { i32, i32 }* %y, { i32, i32 }** %ptr28
  %ptr29 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %24, i32 0, i32 2
  store i1 true, i1* %ptr29
  %ptr30 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %24, i32 0, i32 3
  store { i32 } %23, { i32 }* %ptr30
  %25 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %24
  %e5 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %25, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e5
  %e131 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e1
  %e232 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e2
  %edge = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e131, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge
  %edge33 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e232, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge33
  %tmp = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge33, i32 0, i32 0
  %val = load { i32, i32 }** %tmp
  %val34 = load { i32, i32 }* %val
  %tmp35 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge, i32 0, i32 0
  %val36 = load { i32, i32 }** %tmp35
  %val37 = load { i32, i32 }* %val36
  %strct = alloca { i32, i32 }
  store { i32, i32 } %val37, { i32, i32 }* %strct
  %strct38 = alloca { i32, i32 }
  store { i32, i32 } %val34, { i32, i32 }* %strct38
  %tmp39 = getelementptr inbounds { i32, i32 }* %strct38, i32 0, i32 0
  %val40 = load i32* %tmp39
  %tmp41 = getelementptr inbounds { i32, i32 }* %strct, i32 0, i32 0
  %val42 = load i32* %tmp41
  %tmp43 = icmp eq i32 %val42, %val40
  %tmp44 = getelementptr inbounds { i32, i32 }* %strct38, i32 0, i32 1
  %val45 = load i32* %tmp44
  %tmp46 = getelementptr inbounds { i32, i32 }* %strct, i32 0, i32 1
  %val47 = load i32* %tmp46
  %tmp48 = icmp eq i32 %val47, %val45
  %tmp49 = mul i1 %tmp48, true
  %tmp50 = mul i1 %tmp43, %tmp49
  %tmp51 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge33, i32 0, i32 1
  %val52 = load { i32, i32 }** %tmp51
  %val53 = load { i32, i32 }* %val52
  %tmp54 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge, i32 0, i32 1
  %val55 = load { i32, i32 }** %tmp54
  %val56 = load { i32, i32 }* %val55
  %strct57 = alloca { i32, i32 }
  store { i32, i32 } %val56, { i32, i32 }* %strct57
  %strct58 = alloca { i32, i32 }
  store { i32, i32 } %val53, { i32, i32 }* %strct58
  %tmp59 = getelementptr inbounds { i32, i32 }* %strct58, i32 0, i32 0
  %val60 = load i32* %tmp59
  %tmp61 = getelementptr inbounds { i32, i32 }* %strct57, i32 0, i32 0
  %val62 = load i32* %tmp61
  %tmp63 = icmp eq i32 %val62, %val60
  %tmp64 = getelementptr inbounds { i32, i32 }* %strct58, i32 0, i32 1
  %val65 = load i32* %tmp64
  %tmp66 = getelementptr inbounds { i32, i32 }* %strct57, i32 0, i32 1
  %val67 = load i32* %tmp66
  %tmp68 = icmp eq i32 %val67, %val65
  %tmp69 = mul i1 %tmp68, true
  %tmp70 = mul i1 %tmp63, %tmp69
  %tmp71 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge33, i32 0, i32 2
  %val72 = load i1* %tmp71
  %tmp73 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge, i32 0, i32 2
  %val74 = load i1* %tmp73
  %tmp75 = icmp eq i1 %val74, %val72
  %tmp76 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge33, i32 0, i32 3
  %val77 = load { i32 }* %tmp76
  %tmp78 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge, i32 0, i32 3
  %val79 = load { i32 }* %tmp78
  %strct80 = alloca { i32 }
  store { i32 } %val79, { i32 }* %strct80
  %strct81 = alloca { i32 }
  store { i32 } %val77, { i32 }* %strct81
  %tmp82 = getelementptr inbounds { i32 }* %strct81, i32 0, i32 0
  %val83 = load i32* %tmp82
  %tmp84 = getelementptr inbounds { i32 }* %strct80, i32 0, i32 0
  %val85 = load i32* %tmp84
  %tmp86 = icmp eq i32 %val85, %val83
  %tmp87 = mul i1 %tmp86, true
  %tmp88 = mul i1 %tmp75, %tmp87
  %tmp89 = mul i1 %tmp70, %tmp88
  %tmp90 = mul i1 %tmp50, %tmp89
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt16, i32 0, i32 0), i1 %tmp90)
  %e191 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e1
  %e392 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e3
  %edge93 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e191, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge93
  %edge94 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e392, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge94
  %tmp95 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge94, i32 0, i32 0
  %val96 = load { i32, i32 }** %tmp95
  %val97 = load { i32, i32 }* %val96
  %tmp98 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge93, i32 0, i32 0
  %val99 = load { i32, i32 }** %tmp98
  %val100 = load { i32, i32 }* %val99
  %strct101 = alloca { i32, i32 }
  store { i32, i32 } %val100, { i32, i32 }* %strct101
  %strct102 = alloca { i32, i32 }
  store { i32, i32 } %val97, { i32, i32 }* %strct102
  %tmp103 = getelementptr inbounds { i32, i32 }* %strct102, i32 0, i32 0
  %val104 = load i32* %tmp103
  %tmp105 = getelementptr inbounds { i32, i32 }* %strct101, i32 0, i32 0
  %val106 = load i32* %tmp105
  %tmp107 = icmp eq i32 %val106, %val104
  %tmp108 = getelementptr inbounds { i32, i32 }* %strct102, i32 0, i32 1
  %val109 = load i32* %tmp108
  %tmp110 = getelementptr inbounds { i32, i32 }* %strct101, i32 0, i32 1
  %val111 = load i32* %tmp110
  %tmp112 = icmp eq i32 %val111, %val109
  %tmp113 = mul i1 %tmp112, true
  %tmp114 = mul i1 %tmp107, %tmp113
  %tmp115 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge94, i32 0, i32 1
  %val116 = load { i32, i32 }** %tmp115
  %val117 = load { i32, i32 }* %val116
  %tmp118 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge93, i32 0, i32 1
  %val119 = load { i32, i32 }** %tmp118
  %val120 = load { i32, i32 }* %val119
  %strct121 = alloca { i32, i32 }
  store { i32, i32 } %val120, { i32, i32 }* %strct121
  %strct122 = alloca { i32, i32 }
  store { i32, i32 } %val117, { i32, i32 }* %strct122
  %tmp123 = getelementptr inbounds { i32, i32 }* %strct122, i32 0, i32 0
  %val124 = load i32* %tmp123
  %tmp125 = getelementptr inbounds { i32, i32 }* %strct121, i32 0, i32 0
  %val126 = load i32* %tmp125
  %tmp127 = icmp eq i32 %val126, %val124
  %tmp128 = getelementptr inbounds { i32, i32 }* %strct122, i32 0, i32 1
  %val129 = load i32* %tmp128
  %tmp130 = getelementptr inbounds { i32, i32 }* %strct121, i32 0, i32 1
  %val131 = load i32* %tmp130
  %tmp132 = icmp eq i32 %val131, %val129
  %tmp133 = mul i1 %tmp132, true
  %tmp134 = mul i1 %tmp127, %tmp133
  %tmp135 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge94, i32 0, i32 2
  %val136 = load i1* %tmp135
  %tmp137 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge93, i32 0, i32 2
  %val138 = load i1* %tmp137
  %tmp139 = icmp eq i1 %val138, %val136
  %tmp140 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge94, i32 0, i32 3
  %val141 = load { i32 }* %tmp140
  %tmp142 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge93, i32 0, i32 3
  %val143 = load { i32 }* %tmp142
  %strct144 = alloca { i32 }
  store { i32 } %val143, { i32 }* %strct144
  %strct145 = alloca { i32 }
  store { i32 } %val141, { i32 }* %strct145
  %tmp146 = getelementptr inbounds { i32 }* %strct145, i32 0, i32 0
  %val147 = load i32* %tmp146
  %tmp148 = getelementptr inbounds { i32 }* %strct144, i32 0, i32 0
  %val149 = load i32* %tmp148
  %tmp150 = icmp eq i32 %val149, %val147
  %tmp151 = mul i1 %tmp150, true
  %tmp152 = mul i1 %tmp139, %tmp151
  %tmp153 = mul i1 %tmp134, %tmp152
  %tmp154 = mul i1 %tmp114, %tmp153
  %printf155 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt16, i32 0, i32 0), i1 %tmp154)
  %e1156 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e1
  %e4157 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e4
  %edge158 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e1156, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge158
  %edge159 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e4157, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge159
  %tmp160 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge159, i32 0, i32 0
  %val161 = load { i32, i32 }** %tmp160
  %val162 = load { i32, i32 }* %val161
  %tmp163 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge158, i32 0, i32 0
  %val164 = load { i32, i32 }** %tmp163
  %val165 = load { i32, i32 }* %val164
  %strct166 = alloca { i32, i32 }
  store { i32, i32 } %val165, { i32, i32 }* %strct166
  %strct167 = alloca { i32, i32 }
  store { i32, i32 } %val162, { i32, i32 }* %strct167
  %tmp168 = getelementptr inbounds { i32, i32 }* %strct167, i32 0, i32 0
  %val169 = load i32* %tmp168
  %tmp170 = getelementptr inbounds { i32, i32 }* %strct166, i32 0, i32 0
  %val171 = load i32* %tmp170
  %tmp172 = icmp eq i32 %val171, %val169
  %tmp173 = getelementptr inbounds { i32, i32 }* %strct167, i32 0, i32 1
  %val174 = load i32* %tmp173
  %tmp175 = getelementptr inbounds { i32, i32 }* %strct166, i32 0, i32 1
  %val176 = load i32* %tmp175
  %tmp177 = icmp eq i32 %val176, %val174
  %tmp178 = mul i1 %tmp177, true
  %tmp179 = mul i1 %tmp172, %tmp178
  %tmp180 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge159, i32 0, i32 1
  %val181 = load { i32, i32 }** %tmp180
  %val182 = load { i32, i32 }* %val181
  %tmp183 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge158, i32 0, i32 1
  %val184 = load { i32, i32 }** %tmp183
  %val185 = load { i32, i32 }* %val184
  %strct186 = alloca { i32, i32 }
  store { i32, i32 } %val185, { i32, i32 }* %strct186
  %strct187 = alloca { i32, i32 }
  store { i32, i32 } %val182, { i32, i32 }* %strct187
  %tmp188 = getelementptr inbounds { i32, i32 }* %strct187, i32 0, i32 0
  %val189 = load i32* %tmp188
  %tmp190 = getelementptr inbounds { i32, i32 }* %strct186, i32 0, i32 0
  %val191 = load i32* %tmp190
  %tmp192 = icmp eq i32 %val191, %val189
  %tmp193 = getelementptr inbounds { i32, i32 }* %strct187, i32 0, i32 1
  %val194 = load i32* %tmp193
  %tmp195 = getelementptr inbounds { i32, i32 }* %strct186, i32 0, i32 1
  %val196 = load i32* %tmp195
  %tmp197 = icmp eq i32 %val196, %val194
  %tmp198 = mul i1 %tmp197, true
  %tmp199 = mul i1 %tmp192, %tmp198
  %tmp200 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge159, i32 0, i32 2
  %val201 = load i1* %tmp200
  %tmp202 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge158, i32 0, i32 2
  %val203 = load i1* %tmp202
  %tmp204 = icmp eq i1 %val203, %val201
  %tmp205 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge159, i32 0, i32 3
  %val206 = load { i32 }* %tmp205
  %tmp207 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge158, i32 0, i32 3
  %val208 = load { i32 }* %tmp207
  %strct209 = alloca { i32 }
  store { i32 } %val208, { i32 }* %strct209
  %strct210 = alloca { i32 }
  store { i32 } %val206, { i32 }* %strct210
  %tmp211 = getelementptr inbounds { i32 }* %strct210, i32 0, i32 0
  %val212 = load i32* %tmp211
  %tmp213 = getelementptr inbounds { i32 }* %strct209, i32 0, i32 0
  %val214 = load i32* %tmp213
  %tmp215 = icmp eq i32 %val214, %val212
  %tmp216 = mul i1 %tmp215, true
  %tmp217 = mul i1 %tmp204, %tmp216
  %tmp218 = mul i1 %tmp199, %tmp217
  %tmp219 = mul i1 %tmp179, %tmp218
  %printf220 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt16, i32 0, i32 0), i1 %tmp219)
  %e1221 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e1
  %e5222 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e5
  %edge223 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e1221, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge223
  %edge224 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e5222, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge224
  %tmp225 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge224, i32 0, i32 0
  %val226 = load { i32, i32 }** %tmp225
  %val227 = load { i32, i32 }* %val226
  %tmp228 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge223, i32 0, i32 0
  %val229 = load { i32, i32 }** %tmp228
  %val230 = load { i32, i32 }* %val229
  %strct231 = alloca { i32, i32 }
  store { i32, i32 } %val230, { i32, i32 }* %strct231
  %strct232 = alloca { i32, i32 }
  store { i32, i32 } %val227, { i32, i32 }* %strct232
  %tmp233 = getelementptr inbounds { i32, i32 }* %strct232, i32 0, i32 0
  %val234 = load i32* %tmp233
  %tmp235 = getelementptr inbounds { i32, i32 }* %strct231, i32 0, i32 0
  %val236 = load i32* %tmp235
  %tmp237 = icmp eq i32 %val236, %val234
  %tmp238 = getelementptr inbounds { i32, i32 }* %strct232, i32 0, i32 1
  %val239 = load i32* %tmp238
  %tmp240 = getelementptr inbounds { i32, i32 }* %strct231, i32 0, i32 1
  %val241 = load i32* %tmp240
  %tmp242 = icmp eq i32 %val241, %val239
  %tmp243 = mul i1 %tmp242, true
  %tmp244 = mul i1 %tmp237, %tmp243
  %tmp245 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge224, i32 0, i32 1
  %val246 = load { i32, i32 }** %tmp245
  %val247 = load { i32, i32 }* %val246
  %tmp248 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge223, i32 0, i32 1
  %val249 = load { i32, i32 }** %tmp248
  %val250 = load { i32, i32 }* %val249
  %strct251 = alloca { i32, i32 }
  store { i32, i32 } %val250, { i32, i32 }* %strct251
  %strct252 = alloca { i32, i32 }
  store { i32, i32 } %val247, { i32, i32 }* %strct252
  %tmp253 = getelementptr inbounds { i32, i32 }* %strct252, i32 0, i32 0
  %val254 = load i32* %tmp253
  %tmp255 = getelementptr inbounds { i32, i32 }* %strct251, i32 0, i32 0
  %val256 = load i32* %tmp255
  %tmp257 = icmp eq i32 %val256, %val254
  %tmp258 = getelementptr inbounds { i32, i32 }* %strct252, i32 0, i32 1
  %val259 = load i32* %tmp258
  %tmp260 = getelementptr inbounds { i32, i32 }* %strct251, i32 0, i32 1
  %val261 = load i32* %tmp260
  %tmp262 = icmp eq i32 %val261, %val259
  %tmp263 = mul i1 %tmp262, true
  %tmp264 = mul i1 %tmp257, %tmp263
  %tmp265 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge224, i32 0, i32 2
  %val266 = load i1* %tmp265
  %tmp267 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge223, i32 0, i32 2
  %val268 = load i1* %tmp267
  %tmp269 = icmp eq i1 %val268, %val266
  %tmp270 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge224, i32 0, i32 3
  %val271 = load { i32 }* %tmp270
  %tmp272 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge223, i32 0, i32 3
  %val273 = load { i32 }* %tmp272
  %strct274 = alloca { i32 }
  store { i32 } %val273, { i32 }* %strct274
  %strct275 = alloca { i32 }
  store { i32 } %val271, { i32 }* %strct275
  %tmp276 = getelementptr inbounds { i32 }* %strct275, i32 0, i32 0
  %val277 = load i32* %tmp276
  %tmp278 = getelementptr inbounds { i32 }* %strct274, i32 0, i32 0
  %val279 = load i32* %tmp278
  %tmp280 = icmp eq i32 %val279, %val277
  %tmp281 = mul i1 %tmp280, true
  %tmp282 = mul i1 %tmp269, %tmp281
  %tmp283 = mul i1 %tmp264, %tmp282
  %tmp284 = mul i1 %tmp244, %tmp283
  %printf285 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt16, i32 0, i32 0), i1 %tmp284)
  ret i32 0
}
