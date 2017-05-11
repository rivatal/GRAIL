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
  store i32 4, i32* %ptr3
  %7 = load { i32 }* %6
  %d = alloca { i32 }
  store { i32 } %7, { i32 }* %d
  %8 = alloca { i32 }
  %ptr4 = getelementptr inbounds { i32 }* %8, i32 0, i32 0
  store i32 5, i32* %ptr4
  %9 = load { i32 }* %8
  %e = alloca { i32 }
  store { i32 } %9, { i32 }* %e
  %10 = alloca { i32 }
  %ptr5 = getelementptr inbounds { i32 }* %10, i32 0, i32 0
  store i32 25, i32* %ptr5
  %11 = load { i32 }* %10
  %12 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr6 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %12, i32 0, i32 0
  store { i32 }* %a, { i32 }** %ptr6
  %ptr7 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %12, i32 0, i32 1
  store { i32 }* %d, { i32 }** %ptr7
  %ptr8 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %12, i32 0, i32 2
  store i1 true, i1* %ptr8
  %ptr9 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %12, i32 0, i32 3
  store { i32 } %11, { i32 }* %ptr9
  %13 = load { { i32 }*, { i32 }*, i1, { i32 } }* %12
  %x = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  store { { i32 }*, { i32 }*, i1, { i32 } } %13, { { i32 }*, { i32 }*, i1, { i32 } }* %x
  %14 = alloca { i32 }
  %ptr10 = getelementptr inbounds { i32 }* %14, i32 0, i32 0
  store i32 15, i32* %ptr10
  %15 = load { i32 }* %14
  %16 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr11 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %16, i32 0, i32 0
  store { i32 }* %a, { i32 }** %ptr11
  %ptr12 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %16, i32 0, i32 1
  store { i32 }* %c, { i32 }** %ptr12
  %ptr13 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %16, i32 0, i32 2
  store i1 true, i1* %ptr13
  %ptr14 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %16, i32 0, i32 3
  store { i32 } %15, { i32 }* %ptr14
  %17 = load { { i32 }*, { i32 }*, i1, { i32 } }* %16
  %y = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  store { { i32 }*, { i32 }*, i1, { i32 } } %17, { { i32 }*, { i32 }*, i1, { i32 } }* %y
  %18 = alloca { i32 }
  %ptr15 = getelementptr inbounds { i32 }* %18, i32 0, i32 0
  store i32 20, i32* %ptr15
  %19 = load { i32 }* %18
  %20 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr16 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %20, i32 0, i32 0
  store { i32 }* %b, { i32 }** %ptr16
  %ptr17 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %20, i32 0, i32 1
  store { i32 }* %e, { i32 }** %ptr17
  %ptr18 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %20, i32 0, i32 2
  store i1 true, i1* %ptr18
  %ptr19 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %20, i32 0, i32 3
  store { i32 } %19, { i32 }* %ptr19
  %21 = load { { i32 }*, { i32 }*, i1, { i32 } }* %20
  %z = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  store { { i32 }*, { i32 }*, i1, { i32 } } %21, { { i32 }*, { i32 }*, i1, { i32 } }* %z
  %22 = alloca { i32 }
  %ptr20 = getelementptr inbounds { i32 }* %22, i32 0, i32 0
  store i32 30, i32* %ptr20
  %23 = load { i32 }* %22
  %24 = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  %ptr21 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %24, i32 0, i32 0
  store { i32 }* %d, { i32 }** %ptr21
  %ptr22 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %24, i32 0, i32 1
  store { i32 }* %e, { i32 }** %ptr22
  %ptr23 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %24, i32 0, i32 2
  store i1 true, i1* %ptr23
  %ptr24 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %24, i32 0, i32 3
  store { i32 } %23, { i32 }* %ptr24
  %25 = load { { i32 }*, { i32 }*, i1, { i32 } }* %24
  %m = alloca { { i32 }*, { i32 }*, i1, { i32 } }
  store { { i32 }*, { i32 }*, i1, { i32 } } %25, { { i32 }*, { i32 }*, i1, { i32 } }* %m
  %26 = alloca { i32 }
  %ptr25 = getelementptr inbounds { i32 }* %26, i32 0, i32 0
  store i32 2, i32* %ptr25
  %27 = load { i32 }* %26
  %g = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  %ptr26 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 2
  store { i32 } %27, { i32 }* %ptr26
  %e27 = load { i32 }* %e
  %d28 = load { i32 }* %d
  %c29 = load { i32 }* %c
  %b30 = load { i32 }* %b
  %a31 = load { i32 }* %a
  %strct = alloca { { i32 }*, i32 }
  %lst = alloca { i32 }, i32 5
  %ptr32 = getelementptr inbounds { i32 }* %lst, i32 0
  store { i32 } %e27, { i32 }* %ptr32
  %ptr33 = getelementptr inbounds { i32 }* %lst, i32 1
  store { i32 } %d28, { i32 }* %ptr33
  %ptr34 = getelementptr inbounds { i32 }* %lst, i32 2
  store { i32 } %c29, { i32 }* %ptr34
  %ptr35 = getelementptr inbounds { i32 }* %lst, i32 3
  store { i32 } %b30, { i32 }* %ptr35
  %ptr36 = getelementptr inbounds { i32 }* %lst, i32 4
  store { i32 } %a31, { i32 }* %ptr36
  %p0 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 1
  store { i32 }* %lst, { i32 }** %p0
  store i32 5, i32* %p1
  %lst37 = load { { i32 }*, i32 }* %strct
  %z38 = load { { i32 }*, { i32 }*, i1, { i32 } }* %z
  %y39 = load { { i32 }*, { i32 }*, i1, { i32 } }* %y
  %x40 = load { { i32 }*, { i32 }*, i1, { i32 } }* %x
  %strct41 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %lst42 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 3
  %ptr43 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst42, i32 0
  store { { i32 }*, { i32 }*, i1, { i32 } } %z38, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr43
  %ptr44 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst42, i32 1
  store { { i32 }*, { i32 }*, i1, { i32 } } %y39, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr44
  %ptr45 = getelementptr inbounds { { i32 }*, { i32 }*, i1, { i32 } }* %lst42, i32 2
  store { { i32 }*, { i32 }*, i1, { i32 } } %x40, { { i32 }*, { i32 }*, i1, { i32 } }* %ptr45
  %p046 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct41, i32 0, i32 0
  %p147 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct41, i32 0, i32 1
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst42, { { i32 }*, { i32 }*, i1, { i32 } }** %p046
  store i32 3, i32* %p147
  %lst48 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct41
  %ptr49 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 0
  store { { i32 }*, i32 } %lst37, { { i32 }*, i32 }* %ptr49
  %ptr50 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %lst48, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr50
  %g51 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g
  %graph = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g51, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %graph
  ret i32 0
}
