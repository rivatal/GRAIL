; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

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
  %a11 = load i32, i32* %a1
  %b12 = load i32, i32* %b2
  %tmp13 = icmp ne i32 %a11, %b12
  br i1 %tmp13, label %while_body, label %merge14

while_body:                                       ; preds = %while
  %a3 = load i32, i32* %a1
  %b4 = load i32, i32* %b2
  %tmp = icmp sgt i32 %a3, %b4
  br i1 %tmp, label %then, label %else

merge:                                            ; preds = %else, %then
  br label %while

then:                                             ; preds = %while_body
  %a5 = load i32, i32* %a1
  %b6 = load i32, i32* %b2
  %tmp7 = sub i32 %a5, %b6
  store i32 %tmp7, i32* %a1
  br label %merge

else:                                             ; preds = %while_body
  %b8 = load i32, i32* %b2
  %a9 = load i32, i32* %a1
  %tmp10 = sub i32 %b8, %a9
  store i32 %tmp10, i32* %b2
  br label %merge

merge14:                                          ; preds = %while
  %a15 = load i32, i32* %a1
  ret i32 %a15
}

define i32 @main() {
entry:
  %"gcd!1_result" = call i32 @"gcd!1"(i32 2, i32 14)
  ret i32 0
}
