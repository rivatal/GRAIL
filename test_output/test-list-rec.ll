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
  %2 = alloca { i32, i32 }
  %ptr2 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 0
  store i32 3, i32* %ptr2
  %ptr3 = getelementptr inbounds { i32, i32 }* %2, i32 0, i32 1
  store i32 4, i32* %ptr3
  %3 = load { i32, i32 }* %2
  %strct = alloca { { i32, i32 }*, i32 }
  %lst = alloca { i32, i32 }, i32 2
  %ptr4 = getelementptr inbounds { i32, i32 }* %lst, i32 0
  store { i32, i32 } %1, { i32, i32 }* %ptr4
  %ptr5 = getelementptr inbounds { i32, i32 }* %lst, i32 1
  store { i32, i32 } %3, { i32, i32 }* %ptr5
  %p0 = getelementptr inbounds { { i32, i32 }*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { { i32, i32 }*, i32 }* %strct, i32 0, i32 1
  store { i32, i32 }* %lst, { i32, i32 }** %p0
  store i32 2, i32* %p1
  %lst6 = load { { i32, i32 }*, i32 }* %strct
  %list = alloca { { i32, i32 }*, i32 }
  store { { i32, i32 }*, i32 } %lst6, { { i32, i32 }*, i32 }* %list
  ret i32 0
}
