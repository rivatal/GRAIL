; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"printint!1"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define void @"printint!2"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

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
  %a = alloca { i32*, i32 }
  store { i32*, i32 } %lst3, { i32*, i32 }* %a
  %lst4 = alloca { i32*, i32 }
  %a5 = load { i32*, i32 }* %a
  store { i32*, i32 } %a5, { i32*, i32 }* %lst4
  %x = alloca i32
  %tmp = getelementptr inbounds { i32*, i32 }* %lst4, i32 0, i32 0
  %ar = load i32** %tmp
  %tmp6 = getelementptr inbounds { i32*, i32 }* %lst4, i32 0, i32 1
  %end = load i32* %tmp6
  %ind = alloca i32
  store i32 0, i32* %ind
  br label %while

while:                                            ; preds = %while_body, %entry
  %tmp11 = load i32* %ind
  %comp = icmp slt i32 %tmp11, %end
  br i1 %comp, label %while_body, label %merge

while_body:                                       ; preds = %while
  %i = load i32* %ind
  %ptr7 = getelementptr inbounds i32* %ar, i32 %i
  %tmp8 = load i32* %ptr7
  store i32 %tmp8, i32* %x
  %x9 = load i32* %x
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i32 %x9)
  %tmp10 = load i32* %ind
  %inc = add i32 %tmp10, 1
  store i32 %inc, i32* %ind
  br label %while

merge:                                            ; preds = %while
  ret i32 0
}
