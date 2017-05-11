; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [5 x i8] c"yeah\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %0 = alloca { i8*, i32 }
  %ptr = getelementptr inbounds { i8*, i32 }* %0, i32 0, i32 0
  store i8* getelementptr inbounds ([5 x i8]* @str, i32 0, i32 0), i8** %ptr
  %ptr1 = getelementptr inbounds { i8*, i32 }* %0, i32 0, i32 1
  store i32 2, i32* %ptr1
  %1 = load { i8*, i32 }* %0
  %myrec = alloca { i8*, i32 }
  store { i8*, i32 } %1, { i8*, i32 }* %myrec
  %ext_val = getelementptr inbounds { i8*, i32 }* %myrec, i32 0, i32 0
  %2 = load i8** %ext_val
  %printf = call i32 (i8*, ...)* @printf(i8* %2)
  %ext_val2 = getelementptr inbounds { i8*, i32 }* %myrec, i32 0, i32 1
  %3 = load i32* %ext_val2
  %printf3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt, i32 0, i32 0), i32 %3)
  ret i32 0
}
