; ModuleID = 'Grail'

%struct.6 = type { i32, i32 }

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"printint!1"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define void @main() {
entry:
  %0 = alloca %struct.6
  %ptr = getelementptr inbounds %struct.6, %struct.6* %0, i32 0, i32 0
  store i32 1, i32* %ptr
  %ptr1 = getelementptr inbounds %struct.6, %struct.6* %0, i32 0, i32 1
  store i32 2, i32* %ptr1
  %1 = load %struct.6, %struct.6* %0
  %myrec = alloca %struct.6
  store %struct.6 %1, %struct.6* %myrec
  %ext_val = getelementptr inbounds %struct.6, %struct.6* %myrec, i32 0, i32 0
  %2 = load i32, i32* %ext_val
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), i32 %2)
  ret void
}
