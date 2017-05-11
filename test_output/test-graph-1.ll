; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [5 x i8] c"word\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %0 = alloca { i8* }
  %ptr = getelementptr inbounds { i8* }* %0, i32 0, i32 0
  store i8* getelementptr inbounds ([5 x i8]* @str, i32 0, i32 0), i8** %ptr
  %1 = load { i8* }* %0
  %x = alloca { i8* }
  store { i8* } %1, { i8* }* %x
  %2 = alloca { i32 }
  %ptr1 = getelementptr inbounds { i32 }* %2, i32 0, i32 0
  store i32 1, i32* %ptr1
  %3 = load { i32 }* %2
  %g = alloca { { { i8* }*, i32 }, { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }, { i32 } }
  %ptr2 = getelementptr inbounds { { { i8* }*, i32 }, { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 2
  store { i32 } %3, { i32 }* %ptr2
  %x3 = load { i8* }* %x
  %strct = alloca { { i8* }*, i32 }
  %lst = alloca { i8* }
  %ptr4 = getelementptr inbounds { i8* }* %lst, i32 0
  store { i8* } %x3, { i8* }* %ptr4
  %p0 = getelementptr inbounds { { i8* }*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { { i8* }*, i32 }* %strct, i32 0, i32 1
  store { i8* }* %lst, { i8* }** %p0
  store i32 1, i32* %p1
  %lst5 = load { { i8* }*, i32 }* %strct
  %4 = alloca { i32 }
  %ptr6 = getelementptr inbounds { i32 }* %4, i32 0, i32 0
  store i32 1, i32* %ptr6
  %5 = load { i32 }* %4
  %6 = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  %ptr7 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 0
  store { i8* }* %x, { i8* }** %ptr7
  %ptr8 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 1
  store { i8* }* %x, { i8* }** %ptr8
  %ptr9 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 2
  store i1 true, i1* %ptr9
  %ptr10 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %6, i32 0, i32 3
  store { i32 } %5, { i32 }* %ptr10
  %7 = load { { i8* }*, { i8* }*, i1, { i32 } }* %6
  %strct11 = alloca { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }
  %lst12 = alloca { { i8* }*, { i8* }*, i1, { i32 } }
  %ptr13 = getelementptr inbounds { { i8* }*, { i8* }*, i1, { i32 } }* %lst12, i32 0
  store { { i8* }*, { i8* }*, i1, { i32 } } %7, { { i8* }*, { i8* }*, i1, { i32 } }* %ptr13
  %p014 = getelementptr inbounds { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }* %strct11, i32 0, i32 0
  %p115 = getelementptr inbounds { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }* %strct11, i32 0, i32 1
  store { { i8* }*, { i8* }*, i1, { i32 } }* %lst12, { { i8* }*, { i8* }*, i1, { i32 } }** %p014
  store i32 1, i32* %p115
  %lst16 = load { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }* %strct11
  %ptr17 = getelementptr inbounds { { { i8* }*, i32 }, { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 0
  store { { i8* }*, i32 } %lst5, { { i8* }*, i32 }* %ptr17
  %ptr18 = getelementptr inbounds { { { i8* }*, i32 }, { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 1
  store { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 } %lst16, { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }* %ptr18
  %g19 = load { { { i8* }*, i32 }, { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g
  %g20 = alloca { { { i8* }*, i32 }, { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i8* }*, i32 }, { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }, { i32 } } %g19, { { { i8* }*, i32 }, { { { i8* }*, { i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g20
  ret i32 0
}
