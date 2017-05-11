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
  %4 = alloca { i32 }
  %ptr4 = getelementptr inbounds { i32 }* %4, i32 0, i32 0
  store i32 1, i32* %ptr4
  %5 = load { i32 }* %4
  %6 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr5 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %6, i32 0, i32 0
  store { i32, i32 }* %x, { i32, i32 }** %ptr5
  %ptr6 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %6, i32 0, i32 1
  store { i32, i32 }* %y, { i32, i32 }** %ptr6
  %ptr7 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %6, i32 0, i32 2
  store i1 true, i1* %ptr7
  %ptr8 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %6, i32 0, i32 3
  store { i32 } %5, { i32 }* %ptr8
  %7 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %6
  %e1 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %7, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e1
  ret i32 0
}
