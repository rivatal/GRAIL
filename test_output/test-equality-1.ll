; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

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
  %e116 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e1
  %e217 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e2
  %edge = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e116, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge
  %edge18 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e217, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge18
  %tmp = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge18, i32 0, i32 0
  %val = load { i32, i32 }** %tmp
  %val19 = load { i32, i32 }* %val
  %tmp20 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge, i32 0, i32 0
  %val21 = load { i32, i32 }** %tmp20
  %val22 = load { i32, i32 }* %val21
  %strct = alloca { i32, i32 }
  store { i32, i32 } %val22, { i32, i32 }* %strct
  %strct23 = alloca { i32, i32 }
  store { i32, i32 } %val19, { i32, i32 }* %strct23
  %tmp24 = getelementptr inbounds { i32, i32 }* %strct23, i32 0, i32 0
  %val25 = load i32* %tmp24
  %tmp26 = getelementptr inbounds { i32, i32 }* %strct, i32 0, i32 0
  %val27 = load i32* %tmp26
  %tmp28 = icmp eq i32 %val27, %val25
  %tmp29 = getelementptr inbounds { i32, i32 }* %strct23, i32 0, i32 1
  %val30 = load i32* %tmp29
  %tmp31 = getelementptr inbounds { i32, i32 }* %strct, i32 0, i32 1
  %val32 = load i32* %tmp31
  %tmp33 = icmp eq i32 %val32, %val30
  %tmp34 = mul i1 %tmp33, true
  %tmp35 = mul i1 %tmp28, %tmp34
  %tmp36 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge18, i32 0, i32 1
  %val37 = load { i32, i32 }** %tmp36
  %val38 = load { i32, i32 }* %val37
  %tmp39 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge, i32 0, i32 1
  %val40 = load { i32, i32 }** %tmp39
  %val41 = load { i32, i32 }* %val40
  %strct42 = alloca { i32, i32 }
  store { i32, i32 } %val41, { i32, i32 }* %strct42
  %strct43 = alloca { i32, i32 }
  store { i32, i32 } %val38, { i32, i32 }* %strct43
  %tmp44 = getelementptr inbounds { i32, i32 }* %strct43, i32 0, i32 0
  %val45 = load i32* %tmp44
  %tmp46 = getelementptr inbounds { i32, i32 }* %strct42, i32 0, i32 0
  %val47 = load i32* %tmp46
  %tmp48 = icmp eq i32 %val47, %val45
  %tmp49 = getelementptr inbounds { i32, i32 }* %strct43, i32 0, i32 1
  %val50 = load i32* %tmp49
  %tmp51 = getelementptr inbounds { i32, i32 }* %strct42, i32 0, i32 1
  %val52 = load i32* %tmp51
  %tmp53 = icmp eq i32 %val52, %val50
  %tmp54 = mul i1 %tmp53, true
  %tmp55 = mul i1 %tmp48, %tmp54
  %tmp56 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge18, i32 0, i32 2
  %val57 = load i1* %tmp56
  %tmp58 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge, i32 0, i32 2
  %val59 = load i1* %tmp58
  %tmp60 = icmp eq i1 %val59, %val57
  %tmp61 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge18, i32 0, i32 3
  %val62 = load { i32 }* %tmp61
  %tmp63 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %edge, i32 0, i32 3
  %val64 = load { i32 }* %tmp63
  %strct65 = alloca { i32 }
  store { i32 } %val64, { i32 }* %strct65
  %strct66 = alloca { i32 }
  store { i32 } %val62, { i32 }* %strct66
  %tmp67 = getelementptr inbounds { i32 }* %strct66, i32 0, i32 0
  %val68 = load i32* %tmp67
  %tmp69 = getelementptr inbounds { i32 }* %strct65, i32 0, i32 0
  %val70 = load i32* %tmp69
  %tmp71 = icmp eq i32 %val70, %val68
  %tmp72 = mul i1 %tmp71, true
  %tmp73 = mul i1 %tmp60, %tmp72
  %tmp74 = mul i1 %tmp55, %tmp73
  %tmp75 = mul i1 %tmp35, %tmp74
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i1 %tmp75)
  ret i32 0
}
