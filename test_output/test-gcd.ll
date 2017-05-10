; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt6 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt7 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt8 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt9 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @"gcd!1"(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %b2 = alloca i32
  store i32 %b, i32* %b2
  br label %while

while:                                            ; preds = %merge, %entry
  %a11 = load i32* %a1
  %b12 = load i32* %b2
  %tmp13 = icmp eq i32 %a11, %b12
  %tmp14 = sub i1 true, %tmp13
  br i1 %tmp14, label %while_body, label %merge15

while_body:                                       ; preds = %while
  %a3 = load i32* %a1
  %b4 = load i32* %b2
  %tmp = icmp sgt i32 %a3, %b4
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else, %then
  br label %while

then:                                             ; preds = %while_body
  %a5 = load i32* %a1
  %b6 = load i32* %b2
  %tmp7 = sub i32 %a5, %b6
  store i32 %tmp7, i32* %a1
  br label %merge

else:                                             ; preds = %while_body
  %b8 = load i32* %b2
  %a9 = load i32* %a1
  %tmp10 = sub i32 %b8, %a9
  store i32 %tmp10, i32* %b2
  br label %merge

merge15:                                          ; preds = %while
  %a16 = load i32* %a1
  ret i32 %a16
}

define void @"printint!2"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define i32 @"gcd!3"(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %b2 = alloca i32
  store i32 %b, i32* %b2
  br label %while

while:                                            ; preds = %merge, %entry
  %a11 = load i32* %a1
  %b12 = load i32* %b2
  %tmp13 = icmp eq i32 %a11, %b12
  %tmp14 = sub i1 true, %tmp13
  br i1 %tmp14, label %while_body, label %merge15

while_body:                                       ; preds = %while
  %a3 = load i32* %a1
  %b4 = load i32* %b2
  %tmp = icmp sgt i32 %a3, %b4
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else, %then
  br label %while

then:                                             ; preds = %while_body
  %a5 = load i32* %a1
  %b6 = load i32* %b2
  %tmp7 = sub i32 %a5, %b6
  store i32 %tmp7, i32* %a1
  br label %merge

else:                                             ; preds = %while_body
  %b8 = load i32* %b2
  %a9 = load i32* %a1
  %tmp10 = sub i32 %b8, %a9
  store i32 %tmp10, i32* %b2
  br label %merge

merge15:                                          ; preds = %while
  %a16 = load i32* %a1
  ret i32 %a16
}

define void @"printint!4"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define i32 @main() {
entry:
  %"gcd!3_result" = call i32 @"gcd!3"(i32 2, i32 14)
  %c = alloca i32
  store i32 %"gcd!3_result", i32* %c
  %c1 = load i32* %c
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt8, i32 0, i32 0), i32 %c1)
  ret i32 0
}
