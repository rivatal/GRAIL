; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %strct = alloca { i32*, i32 }
  %lst = alloca i32, i32 3
  %ptr = getelementptr inbounds i32* %lst, i32 0
  store i32 1, i32* %ptr
  %ptr1 = getelementptr inbounds i32* %lst, i32 1
  store i32 2, i32* %ptr1
  %ptr2 = getelementptr inbounds i32* %lst, i32 2
  store i32 3, i32* %ptr2
  %p0 = getelementptr inbounds { i32*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { i32*, i32 }* %strct, i32 0, i32 1
  store i32* %lst, i32** %p0
  store i32 3, i32* %p1
  %lst3 = load { i32*, i32 }* %strct
  %x = alloca { i32*, i32 }
  store { i32*, i32 } %lst3, { i32*, i32 }* %x
  %x4 = load { i32*, i32 }* %x
  %y = alloca { i32*, i32 }
  store { i32*, i32 } %x4, { i32*, i32 }* %y
  %tmp = getelementptr inbounds { i32*, i32 }* %y, i32 0, i32 0
  %tmpar = load i32** %tmp
  %ptr5 = getelementptr inbounds i32* %tmpar, i32 0
  store i32 5, i32* %ptr5
  %tmp6 = getelementptr inbounds { i32*, i32 }* %x, i32 0, i32 0
  %tmpar7 = load i32** %tmp6
  %ptr8 = getelementptr inbounds i32* %tmpar7, i32 0
  %item = load i32* %ptr8
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), i32 %item)
  ret i32 0
}
