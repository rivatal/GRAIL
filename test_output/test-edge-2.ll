; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

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
  store i32 2, i32* %ptr11
  %11 = load { i32 }* %10
  %12 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr12 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %12, i32 0, i32 0
  store { i32, i32 }* %x, { i32, i32 }** %ptr12
  %ptr13 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %12, i32 0, i32 1
  store { i32, i32 }* %z, { i32, i32 }** %ptr13
  %ptr14 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %12, i32 0, i32 2
  store i1 true, i1* %ptr14
  %ptr15 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %12, i32 0, i32 3
  store { i32 } %11, { i32 }* %ptr15
  %13 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %12
  %e2 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %13, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e2
  %14 = alloca { i32 }
  %ptr16 = getelementptr inbounds { i32 }* %14, i32 0, i32 0
  store i32 3, i32* %ptr16
  %15 = load { i32 }* %14
  %16 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr17 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %16, i32 0, i32 0
  store { i32, i32 }* %y, { i32, i32 }** %ptr17
  %ptr18 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %16, i32 0, i32 1
  store { i32, i32 }* %z, { i32, i32 }** %ptr18
  %ptr19 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %16, i32 0, i32 2
  store i1 true, i1* %ptr19
  %ptr20 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %16, i32 0, i32 3
  store { i32 } %15, { i32 }* %ptr20
  %17 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %16
  %e3 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %17, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e3
  ret i32 0
}
