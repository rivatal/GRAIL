; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [2 x i8] c"h\00"
@str2 = private unnamed_addr constant [2 x i8] c"f\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str5 = private unnamed_addr constant [2 x i8] c"h\00"
@str6 = private unnamed_addr constant [2 x i8] c"f\00"
@fmt7 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt8 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str9 = private unnamed_addr constant [2 x i8] c"h\00"
@str10 = private unnamed_addr constant [2 x i8] c"f\00"
@fmt11 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt12 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define { i8* } @f() {
entry:
  %0 = alloca { i8* }
  %ptr = getelementptr inbounds { i8* }* %0, i32 0, i32 0
  store i8* getelementptr inbounds ([2 x i8]* @str, i32 0, i32 0), i8** %ptr
  %1 = load { i8* }* %0
  %x = alloca { i8* }
  store { i8* } %1, { i8* }* %x
  %2 = alloca { i8* }
  %ptr1 = getelementptr inbounds { i8* }* %2, i32 0, i32 0
  store i8* getelementptr inbounds ([2 x i8]* @str2, i32 0, i32 0), i8** %ptr1
  %3 = load { i8* }* %2
  %y = alloca { i8* }
  store { i8* } %3, { i8* }* %y
  %4 = alloca { i32 }
  %ptr2 = getelementptr inbounds { i32 }* %4, i32 0, i32 0
  store i32 1, i32* %ptr2
  %5 = load { i32 }* %4
  %6 = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  %ptr3 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 0
  store { i8* }* %x, { i8* }** %ptr3
  %ptr4 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 1
  store { i8* }* %y, { i8* }** %ptr4
  %ptr5 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 2
  store i1 false, i1* %ptr5
  %ptr6 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 3
  store { i32 } %5, { i32 }* %ptr6
  %7 = load { { i8* }*, { i8* }*, i1, { i32 } }* %6
  %e = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  store { { i8* }*, { i8* }*, i1, { i32 } } %7, { { i8* }*, { i8* }*, i1, { i32 } }* %e
  %e7 = load { { i8* }*, { i8* }*, i1, { i32 } }* %e
  %e8 = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  store { { i8* }*, { i8* }*, i1, { i32 } } %e7, { { i8* }*, { i8* }*, i1, { i32 } }* %e8
  %ptr9 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %e8, i32 0, i32 0
  %from = load { i8* }** %ptr9
  %from10 = load { i8* }* %from
  ret { i8* } %from10
}

define { i8* } @"f!1"() {
entry:
  %0 = alloca { i8* }
  %ptr = getelementptr inbounds { i8* }* %0, i32 0, i32 0
  store i8* getelementptr inbounds ([2 x i8]* @str5, i32 0, i32 0), i8** %ptr
  %1 = load { i8* }* %0
  %x = alloca { i8* }
  store { i8* } %1, { i8* }* %x
  %2 = alloca { i8* }
  %ptr1 = getelementptr inbounds { i8* }* %2, i32 0, i32 0
  store i8* getelementptr inbounds ([2 x i8]* @str6, i32 0, i32 0), i8** %ptr1
  %3 = load { i8* }* %2
  %y = alloca { i8* }
  store { i8* } %3, { i8* }* %y
  %4 = alloca { i32 }
  %ptr2 = getelementptr inbounds { i32 }* %4, i32 0, i32 0
  store i32 1, i32* %ptr2
  %5 = load { i32 }* %4
  %6 = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  %ptr3 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 0
  store { i8* }* %x, { i8* }** %ptr3
  %ptr4 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 1
  store { i8* }* %y, { i8* }** %ptr4
  %ptr5 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 2
  store i1 false, i1* %ptr5
  %ptr6 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 3
  store { i32 } %5, { i32 }* %ptr6
  %7 = load { { i8* }*, { i8* }*, i1, { i32 } }* %6
  %e = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  store { { i8* }*, { i8* }*, i1, { i32 } } %7, { { i8* }*, { i8* }*, i1, { i32 } }* %e
  %e7 = load { { i8* }*, { i8* }*, i1, { i32 } }* %e
  %e8 = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  store { { i8* }*, { i8* }*, i1, { i32 } } %e7, { { i8* }*, { i8* }*, i1, { i32 } }* %e8
  %ptr9 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %e8, i32 0, i32 0
  %from = load { i8* }** %ptr9
  %from10 = load { i8* }* %from
  ret { i8* } %from10
}

define { i8* } @"f!2"() {
entry:
  %0 = alloca { i8* }
  %ptr = getelementptr inbounds { i8* }* %0, i32 0, i32 0
  store i8* getelementptr inbounds ([2 x i8]* @str9, i32 0, i32 0), i8** %ptr
  %1 = load { i8* }* %0
  %x = alloca { i8* }
  store { i8* } %1, { i8* }* %x
  %2 = alloca { i8* }
  %ptr1 = getelementptr inbounds { i8* }* %2, i32 0, i32 0
  store i8* getelementptr inbounds ([2 x i8]* @str10, i32 0, i32 0), i8** %ptr1
  %3 = load { i8* }* %2
  %y = alloca { i8* }
  store { i8* } %3, { i8* }* %y
  %4 = alloca { i32 }
  %ptr2 = getelementptr inbounds { i32 }* %4, i32 0, i32 0
  store i32 1, i32* %ptr2
  %5 = load { i32 }* %4
  %6 = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  %ptr3 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 0
  store { i8* }* %x, { i8* }** %ptr3
  %ptr4 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 1
  store { i8* }* %y, { i8* }** %ptr4
  %ptr5 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 2
  store i1 false, i1* %ptr5
  %ptr6 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 3
  store { i32 } %5, { i32 }* %ptr6
  %7 = load { { i8* }*, { i8* }*, i1, { i32 } }* %6
  %e = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  store { { i8* }*, { i8* }*, i1, { i32 } } %7, { { i8* }*, { i8* }*, i1, { i32 } }* %e
  %e7 = load { { i8* }*, { i8* }*, i1, { i32 } }* %e
  %e8 = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  store { { i8* }*, { i8* }*, i1, { i32 } } %e7, { { i8* }*, { i8* }*, i1, { i32 } }* %e8
  %ptr9 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %e8, i32 0, i32 0
  %from = load { i8* }** %ptr9
  %from10 = load { i8* }* %from
  ret { i8* } %from10
}

define i32 @main() {
entry:
  %"f!2_result" = call { i8* } @"f!2"()
  %y = alloca { i8* }
  store { i8* } %"f!2_result", { i8* }* %y
  ret i32 0
}
