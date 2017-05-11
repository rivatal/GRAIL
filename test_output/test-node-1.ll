; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %0 = alloca { i32 }
  %ptr = getelementptr inbounds { i32 }* %0, i32 0, i32 0
  store i32 3, i32* %ptr
  %1 = load { i32 }* %0
  %y = alloca { i32 }
  store { i32 } %1, { i32 }* %y
  %2 = alloca { i32 }
  %ptr1 = getelementptr inbounds { i32 }* %2, i32 0, i32 0
  store i32 1, i32* %ptr1
  %3 = load { i32 }* %2
  %g = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  %ptr2 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 2
  store { i32 } %3, { i32 }* %ptr2
  %y3 = load { i32 }* %y
  %strct = alloca { { i32 }*, i32 }
  %lst = alloca { i32 }
  %ptr4 = getelementptr inbounds { i32 }* %lst, i32 0
  store { i32 } %y3, { i32 }* %ptr4
  %p0 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 1
  store { i32 }* %lst, { i32 }** %p0
  store i32 1, i32* %p1
  %lst5 = load { { i32 }*, i32 }* %strct
  %strct6 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %lst7 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 0
  %p08 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct6, i32 0, i32 0
  %p19 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct6, i32 0, i32 1
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst7, { { i32 }*, { i32 }*, i1, { i32 } }** %p08
  store i32 0, i32* %p19
  %lst10 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct6
  %ptr11 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 0
  store { { i32 }*, i32 } %lst5, { { i32 }*, i32 }* %ptr11
  %ptr12 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %lst10, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr12
  %g13 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g
  %x = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g13, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %x
  %x14 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %x
  %e = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %x14, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %e
  %ptr15 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %e, i32 0, i32 0
  %nodes = load { { i32 }*, i32 }* %ptr15
  ret i32 0
}
