; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [9 x i8] c"EQ works\00"
@str2 = private unnamed_addr constant [20 x i8] c"EQ 2 does not works\00"
@str3 = private unnamed_addr constant [11 x i8] c"EQ 2 works\00"
@str4 = private unnamed_addr constant [9 x i8] c"LT works\00"
@str5 = private unnamed_addr constant [9 x i8] c"GT works\00"
@str6 = private unnamed_addr constant [10 x i8] c"LEQ works\00"
@str7 = private unnamed_addr constant [10 x i8] c"GEQ works\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  br i1 true, label %then, label %else

merge:                                            ; preds = %else, %then
  br i1 false, label %then2, label %else4

then:                                             ; preds = %entry
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([9 x i8]* @str, i32 0, i32 0))
  br label %merge

else:                                             ; preds = %entry
  br label %merge

merge1:                                           ; preds = %else4, %then2
  br i1 true, label %then7, label %else9

then2:                                            ; preds = %merge
  %printf3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([20 x i8]* @str2, i32 0, i32 0))
  br label %merge1

else4:                                            ; preds = %merge
  %printf5 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([11 x i8]* @str3, i32 0, i32 0))
  br label %merge1

merge6:                                           ; preds = %else9, %then7
  br i1 true, label %then11, label %else13

then7:                                            ; preds = %merge1
  %printf8 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([9 x i8]* @str4, i32 0, i32 0))
  br label %merge6

else9:                                            ; preds = %merge1
  br label %merge6

merge10:                                          ; preds = %else13, %then11
  br i1 true, label %then15, label %else17

then11:                                           ; preds = %merge6
  %printf12 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([9 x i8]* @str5, i32 0, i32 0))
  br label %merge10

else13:                                           ; preds = %merge6
  br label %merge10

merge14:                                          ; preds = %else17, %then15
  br i1 true, label %then19, label %else21

then15:                                           ; preds = %merge10
  %printf16 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([10 x i8]* @str6, i32 0, i32 0))
  br label %merge14

else17:                                           ; preds = %merge10
  br label %merge14

merge18:                                          ; preds = %else21, %then19
  ret i32 0

then19:                                           ; preds = %merge14
  %printf20 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([10 x i8]* @str7, i32 0, i32 0))
  br label %merge18

else21:                                           ; preds = %merge14
  br label %merge18
}
