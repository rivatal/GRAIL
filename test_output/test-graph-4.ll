; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %0 = alloca { i32, i32 }
  %ptr = getelementptr inbounds { i32, i32 }* %0, i32 0, i32 0
  store i32 10, i32* %ptr
  %ptr1 = getelementptr inbounds { i32, i32 }* %0, i32 0, i32 1
  store i32 1, i32* %ptr1
  %1 = load { i32, i32 }* %0
  %a = alloca { i32, i32 }
  store { i32, i32 } %1, { i32, i32 }* %a
  %2 = alloca { i32, i32 }
  %ptr2 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 0
  store i32 10, i32* %ptr2
  %ptr3 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 1
  store i32 2, i32* %ptr3
  %3 = load { i32, i32 }* %2
  %b = alloca { i32, i32 }
  store { i32, i32 } %3, { i32, i32 }* %b
  %4 = alloca { i32, i32 }
  %ptr4 = getelementptr inbounds { i32, i32 }* %4, i32 0, i32 0
  store i32 15, i32* %ptr4
  %ptr5 = getelementptr inbounds { i32, i32 }* %4, i32 0, i32 1
  store i32 3, i32* %ptr5
  %5 = load { i32, i32 }* %4
  %c = alloca { i32, i32 }
  store { i32, i32 } %5, { i32, i32 }* %c
  %6 = alloca { i32, i32 }
  %ptr6 = getelementptr inbounds { i32, i32 }* %6, i32 0, i32 0
  store i32 20, i32* %ptr6
  %ptr7 = getelementptr inbounds { i32, i32 }* %6, i32 0, i32 1
  store i32 4, i32* %ptr7
  %7 = load { i32, i32 }* %6
  %d = alloca { i32, i32 }
  store { i32, i32 } %7, { i32, i32 }* %d
  %8 = alloca { i32 }
  %ptr8 = getelementptr inbounds { i32 }* %8, i32 0, i32 0
  store i32 10, i32* %ptr8
  %9 = load { i32 }* %8
  %10 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr9 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %10, i32 0, i32 0
  store { i32, i32 }* %a, { i32, i32 }** %ptr9
  %ptr10 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %10, i32 0, i32 1
  store { i32, i32 }* %a, { i32, i32 }** %ptr10
  %ptr11 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %10, i32 0, i32 2
  store i1 true, i1* %ptr11
  %ptr12 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %10, i32 0, i32 3
  store { i32 } %9, { i32 }* %ptr12
  %11 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %10
  %x = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %11, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %x
  %12 = alloca { i32 }
  %ptr13 = getelementptr inbounds { i32 }* %12, i32 0, i32 0
  store i32 20, i32* %ptr13
  %13 = load { i32 }* %12
  %14 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  %ptr14 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %14, i32 0, i32 0
  store { i32, i32 }* %b, { i32, i32 }** %ptr14
  %ptr15 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %14, i32 0, i32 1
  store { i32, i32 }* %b, { i32, i32 }** %ptr15
  %ptr16 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %14, i32 0, i32 2
  store i1 true, i1* %ptr16
  %ptr17 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %14, i32 0, i32 3
  store { i32 } %13, { i32 }* %ptr17
  %15 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %14
  %y = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %15, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %y
  %16 = alloca { i32 }
  %ptr18 = getelementptr inbounds { i32 }* %16, i32 0, i32 0
  store i32 2, i32* %ptr18
  %17 = load { i32 }* %16
  %g = alloca { { { i32, i32 }*, i32 }, { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  %ptr19 = getelementptr inbounds { { { i32, i32 }*, i32 }, { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 2
  store { i32 } %17, { i32 }* %ptr19
  %strct = alloca { { i32, i32 }*, i32 }
  %lst = alloca { i32, i32 }, i32 0
  %p0 = getelementptr inbounds { { i32, i32 }*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { { i32, i32 }*, i32 }* %strct, i32 0, i32 1
  store { i32, i32 }* %lst, { i32, i32 }** %p0
  store i32 0, i32* %p1
  %lst20 = load { { i32, i32 }*, i32 }* %strct
  %y21 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %y
  %x22 = load { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %x
  %strct23 = alloca { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }
  %lst24 = alloca { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }, i32 2
  %ptr25 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %lst24, i32 0
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %y21, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %ptr25
  %ptr26 = getelementptr inbounds { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %lst24, i32 1
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } } %x22, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %ptr26
  %p027 = getelementptr inbounds { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }* %strct23, i32 0, i32 0
  %p128 = getelementptr inbounds { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }* %strct23, i32 0, i32 1
  store { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }* %lst24, { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }** %p027
  store i32 2, i32* %p128
  %lst29 = load { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }* %strct23
  %ptr30 = getelementptr inbounds { { { i32, i32 }*, i32 }, { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 0
  store { { i32, i32 }*, i32 } %lst20, { { i32, i32 }*, i32 }* %ptr30
  %ptr31 = getelementptr inbounds { { { i32, i32 }*, i32 }, { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 1
  store { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 } %lst29, { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }* %ptr31
  %g32 = load { { { i32, i32 }*, i32 }, { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g
  %graph = alloca { { { i32, i32 }*, i32 }, { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32, i32 }*, i32 }, { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g32, { { { i32, i32 }*, i32 }, { { { i32, i32 }*, { i32, i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %graph
  ret i32 0
}
