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
  %g = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  %ptr1 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 2
  store { i32 } %1, { i32 }* %ptr1
  %2 = alloca { i32 }
  %ptr2 = getelementptr inbounds { i32 }* %2, i32 0, i32 0
  store i32 1, i32* %ptr2
  %3 = load { i32 }* %2
  %strct = alloca { { i32 }*, i32 }
  %lst = alloca { i32 }
  %ptr3 = getelementptr inbounds { i32 }* %lst, i32 0
  store { i32 } %3, { i32 }* %ptr3
  %p0 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { { i32 }*, i32 }* %strct, i32 0, i32 1
  store { i32 }* %lst, { i32 }** %p0
  store i32 1, i32* %p1
  %lst4 = load { { i32 }*, i32 }* %strct
  %strct5 = alloca { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }
  %lst6 = alloca { { i32 }*, { i32 }*, i1, { i32 } }, i32 0
  %p07 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct5, i32 0, i32 0
  %p18 = getelementptr inbounds { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct5, i32 0, i32 1
  store { { i32 }*, { i32 }*, i1, { i32 } }* %lst6, { { i32 }*, { i32 }*, i1, { i32 } }** %p07
  store i32 0, i32* %p18
  %lst9 = load { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %strct5
  %ptr10 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 0
  store { { i32 }*, i32 } %lst4, { { i32 }*, i32 }* %ptr10
  %ptr11 = getelementptr inbounds { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 1
  store { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 } %lst9, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }* %ptr11
  %g12 = load { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g
  %g13 = alloca { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } } %g12, { { { i32 }*, i32 }, { { { i32 }*, { i32 }*, i1, { i32 } }*, i32 }, { i32 } }* %g13
  ret i32 0
}
