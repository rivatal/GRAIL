; ModuleID = 'Grail'

%struct.6 = type { i32, i32, i32, i32 }
%struct.8 = type { i32 }
%struct.9 = type { %struct.6*, %struct.6*, i1, %struct.8 }
%struct.14 = type { i32, i32 }
%struct.16 = type { i32, i32 }

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
@fmt10 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt11 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

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

define void @"printint!3"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define void @"printint!4"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret void
}

define i32 @"sample_display!5"(i32 %x) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  ret i32 0
}

define i32 @main() {
entry:
  %0 = alloca %struct.6
  %ptr = getelementptr inbounds %struct.6* %0, i32 0, i32 0
  store i32 1, i32* %ptr
  %ptr1 = getelementptr inbounds %struct.6* %0, i32 0, i32 1
  store i32 2, i32* %ptr1
  %ptr2 = getelementptr inbounds %struct.6* %0, i32 0, i32 2
  store i32 3, i32* %ptr2
  %ptr3 = getelementptr inbounds %struct.6* %0, i32 0, i32 3
  store i32 4, i32* %ptr3
  %1 = load %struct.6* %0
  %x = alloca %struct.6
  store %struct.6 %1, %struct.6* %x
  %2 = alloca %struct.8
  %ptr4 = getelementptr inbounds %struct.8* %2, i32 0, i32 0
  store i32 1, i32* %ptr4
  %3 = load %struct.8* %2
  %4 = alloca %struct.9
  %ptr5 = getelementptr inbounds %struct.9* %4, i32 0, i32 0
  store %struct.6* %x, %struct.6** %ptr5
  %ptr6 = getelementptr inbounds %struct.9* %4, i32 0, i32 1
  store %struct.6* %x, %struct.6** %ptr6
  %ptr7 = getelementptr inbounds %struct.9* %4, i32 0, i32 2
  store i1 true, i1* %ptr7
  %ptr8 = getelementptr inbounds %struct.9* %4, i32 0, i32 3
  store %struct.8 %3, %struct.8* %ptr8
  %5 = load %struct.9* %4
  %y = alloca %struct.9
  store %struct.9 %5, %struct.9* %y
  %ext_val = getelementptr inbounds %struct.6* %x, i32 0, i32 0
  %6 = load i32* %ext_val
  %z1 = alloca i32
  store i32 %6, i32* %z1
  %ext_val9 = getelementptr inbounds %struct.6* %x, i32 0, i32 1
  %7 = load i32* %ext_val9
  %z2 = alloca i32
  store i32 %7, i32* %z2
  %ext_val10 = getelementptr inbounds %struct.6* %x, i32 0, i32 2
  %8 = load i32* %ext_val10
  %z3 = alloca i32
  store i32 %8, i32* %z3
  %z111 = load i32* %z1
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt10, i32 0, i32 0), i32 %z111)
  %z212 = load i32* %z2
  %printf13 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt10, i32 0, i32 0), i32 %z212)
  %z314 = load i32* %z3
  %printf15 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt10, i32 0, i32 0), i32 %z314)
  %9 = alloca %struct.14
  %ptr16 = getelementptr inbounds %struct.14* %9, i32 0, i32 0
  store i32 1, i32* %ptr16
  %ptr17 = getelementptr inbounds %struct.14* %9, i32 0, i32 1
  store i32 2, i32* %ptr17
  %10 = load %struct.14* %9
  %e = alloca %struct.14
  store %struct.14 %10, %struct.14* %e
  %ext_val18 = getelementptr inbounds %struct.14* %e, i32 0, i32 0
  %11 = load i32* %ext_val18
  %l = alloca i32
  store i32 %11, i32* %l
  %12 = alloca %struct.16
  %ptr19 = getelementptr inbounds %struct.16* %12, i32 0, i32 0
  store i32 6, i32* %ptr19
  %ptr20 = getelementptr inbounds %struct.16* %12, i32 0, i32 1
  store i32 10, i32* %ptr20
  %13 = load %struct.16* %12
  %e21 = alloca %struct.16
  store %struct.16 %13, %struct.16* %e21
  %ext_val22 = getelementptr inbounds %struct.16* %e21, i32 0, i32 1
  %14 = load i32* %ext_val22
  %printf23 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt10, i32 0, i32 0), i32 %14)
  %sample_display = call i32 @sample_display(i32 0)
  %k = alloca i32
  store i32 %sample_display, i32* %k
  ret i32 0
}
