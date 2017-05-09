updating map func for (myrec: record 6) = record 6{b (2: int)a (yeah: str)}; 
matching x with type: strassigning x to strupdating map func for  (call print(((myrec: record 6).a : str))) : void; 
matching x with type: strmatching x with type: intassigning x to intupdating map func for  (call printint(((myrec: record 6).b : int))) : void; 
matching x with type: intupdating map func for return (0: int); int

; ModuleID = 'Grail'

%struct.6 = type { i8*, i32 }

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [5 x i8] c"yeah\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define void @"print!1"(i8* %x) {
entry:
  %x1 = alloca i8*
  store i8* %x, i8** %x1
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
  %0 = alloca %struct.6
  %ptr = getelementptr inbounds %struct.6* %0, i32 0, i32 0
  store i8* getelementptr inbounds ([5 x i8]* @str, i32 0, i32 0), i8** %ptr
  %ptr1 = getelementptr inbounds %struct.6* %0, i32 0, i32 1
  store i32 2, i32* %ptr1
  %1 = load %struct.6* %0
  %myrec = alloca %struct.6
  store %struct.6 %1, %struct.6* %myrec
  %ext_val = getelementptr inbounds %struct.6* %myrec, i32 0, i32 0
  %2 = load i8** %ext_val
  %printf = call i32 (i8*, ...)* @printf(i8* %2)
  %ext_val2 = getelementptr inbounds %struct.6* %myrec, i32 0, i32 1
  %3 = load i32* %ext_val2
  %printf3 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i32 %3)
  ret i32 0
}
