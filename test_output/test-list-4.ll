; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"iterate!7"({ i32*, i32 } %list) {
entry:
  %list1 = alloca { i32*, i32 }
  store { i32*, i32 } %list, { i32*, i32 }* %list1
  %i = alloca i32
  store i32 0, i32* %i
  br label %while

while:                                            ; preds = %while_body, %entry
  %i5 = load i32* %i
  %list6 = load { i32*, i32 }* %list1
  %strct = alloca { i32*, i32 }
  store { i32*, i32 } %list6, { i32*, i32 }* %strct
  %tmp7 = getelementptr inbounds { i32*, i32 }* %strct, i32 0, i32 1
  %len = load i32* %tmp7
  %tmp8 = icmp slt i32 %i5, %len
  br i1 %tmp8, label %while_body, label %merge

while_body:                                       ; preds = %while
  %tmp = getelementptr inbounds { i32*, i32 }* %list1, i32 0, i32 0
  %tmpar = load i32** %tmp
  %i2 = load i32* %i
  %ptr = getelementptr inbounds i32* %tmpar, i32 %i2
  %item = load i32* %ptr
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), i32 %item)
  %i3 = load i32* %i
  %tmp4 = add i32 %i3, 1
  store i32 %tmp4, i32* %i
  br label %while

merge:                                            ; preds = %while
  ret void
}

define void @"iterate!10"({ i32*, i32 } %list) {
entry:
  %list1 = alloca { i32*, i32 }
  store { i32*, i32 } %list, { i32*, i32 }* %list1
  %i = alloca i32
  store i32 0, i32* %i
  br label %while

while:                                            ; preds = %while_body, %entry
  %i5 = load i32* %i
  %list6 = load { i32*, i32 }* %list1
  %strct = alloca { i32*, i32 }
  store { i32*, i32 } %list6, { i32*, i32 }* %strct
  %tmp7 = getelementptr inbounds { i32*, i32 }* %strct, i32 0, i32 1
  %len = load i32* %tmp7
  %tmp8 = icmp slt i32 %i5, %len
  br i1 %tmp8, label %while_body, label %merge

while_body:                                       ; preds = %while
  %tmp = getelementptr inbounds { i32*, i32 }* %list1, i32 0, i32 0
  %tmpar = load i32** %tmp
  %i2 = load i32* %i
  %ptr = getelementptr inbounds i32* %tmpar, i32 %i2
  %item = load i32* %ptr
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt2, i32 0, i32 0), i32 %item)
  %i3 = load i32* %i
  %tmp4 = add i32 %i3, 1
  store i32 %tmp4, i32* %i
  br label %while

merge:                                            ; preds = %while
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
  %y = alloca { i32*, i32 }
  store { i32*, i32 } %lst3, { i32*, i32 }* %y
  %y4 = load { i32*, i32 }* %y
  call void @"iterate!10"({ i32*, i32 } %y4)
  ret i32 0
}
