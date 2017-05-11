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
  store i32 3, i32* %ptr2
  %ptr3 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 1
  store i32 4, i32* %ptr3
  %3 = load { i32, i32 }* %2
  %y = alloca { i32, i32 }
  store { i32, i32 } %3, { i32, i32 }* %y
  %4 = alloca { i32, i32 }
  %ptr4 = getelementptr inbounds { i32, i32 }* %4, i32 0, i32 0
  store i32 1, i32* %ptr4
  %ptr5 = getelementptr inbounds { i32, i32 }* %4, i32 0, i32 1
  store i32 2, i32* %ptr5
  %5 = load { i32, i32 }* %4
  %p = alloca { i32, i32 }
  store { i32, i32 } %5, { i32, i32 }* %p
  %6 = alloca { i32, i32 }
  %ptr6 = getelementptr inbounds { i32, i32 }* %6, i32 0, i32 0
  store i32 3, i32* %ptr6
  %ptr7 = getelementptr inbounds { i32, i32 }* %6, i32 0, i32 1
  store i32 4, i32* %ptr7
  %7 = load { i32, i32 }* %6
  %q = alloca { i32, i32 }
  store { i32, i32 } %7, { i32, i32 }* %q
  %8 = alloca { i32 }
  %ptr8 = getelementptr inbounds { i32 }* %8, i32 0, i32 0
  store i32 1, i32* %ptr8
  %9 = load { i32 }* %8
  %10 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr9 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %10, i32 0, i32 0
  store { i32, i32 }* %x, { i32, i32 }** %ptr9
  %ptr10 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %10, i32 0, i32 1
  store { i32, i32 }* %y, { i32, i32 }** %ptr10
  %ptr11 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %10, i32 0, i32 2
  store i1 true, i1* %ptr11
  %ptr12 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %10, i32 0, i32 3
  store { i32 } %9, { i32 }* %ptr12
  %11 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %10
  %e1 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %11, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e1
  %12 = alloca { i32 }
  %ptr13 = getelementptr inbounds { i32 }* %12, i32 0, i32 0
  store i32 1, i32* %ptr13
  %13 = load { i32 }* %12
  %14 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr14 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %14, i32 0, i32 0
  store { i32, i32 }* %p, { i32, i32 }** %ptr14
  %ptr15 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %14, i32 0, i32 1
  store { i32, i32 }* %q, { i32, i32 }** %ptr15
  %ptr16 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %14, i32 0, i32 2
  store i1 true, i1* %ptr16
  %ptr17 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %14, i32 0, i32 3
  store { i32 } %13, { i32 }* %ptr17
  %15 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %14
  %e2 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %15, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e2
  %e118 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e1
  %e219 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %e2
  %strct = alloca { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }
  %lst = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }, i32 2
  %ptr20 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %lst, i32 0
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e118, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %ptr20
  %ptr21 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %lst, i32 1
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %e219, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %ptr21
  %p0 = getelementptr inbounds { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }* %strct, i32 0, i32 1
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %lst, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }** %p0
  store i32 2, i32* %p1
  %lst22 = load { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }* %strct
  %list = alloca { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }
  store { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 } %lst22, { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }* %list
  ret i32 0
}
