; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %strct = alloca { i32*, i32 }
  %lst = alloca i32, i32 5
  %ptr = getelementptr inbounds i32* %lst, i32 0
  store i32 1, i32* %ptr
  %ptr1 = getelementptr inbounds i32* %lst, i32 1
  store i32 2, i32* %ptr1
  %ptr2 = getelementptr inbounds i32* %lst, i32 2
  store i32 3, i32* %ptr2
  %ptr3 = getelementptr inbounds i32* %lst, i32 3
  store i32 4, i32* %ptr3
  %ptr4 = getelementptr inbounds i32* %lst, i32 4
  store i32 5, i32* %ptr4
  %p0 = getelementptr inbounds { i32*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { i32*, i32 }* %strct, i32 0, i32 1
  store i32* %lst, i32** %p0
  store i32 5, i32* %p1
  %lst5 = load { i32*, i32 }* %strct
  %a = alloca { i32*, i32 }
  store { i32*, i32 } %lst5, { i32*, i32 }* %a
  %tmp = getelementptr inbounds { i32*, i32 }* %a, i32 0, i32 0
  %tmpar = load i32** %tmp
  %ptr6 = getelementptr inbounds i32* %tmpar, i32 4
  %item = load i32* %ptr6
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), i32 %item)
  ret i32 0
}