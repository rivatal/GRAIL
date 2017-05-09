matching x with type: intassigning x to intupdating map func for  (call printint(((a: int) + (3: int): int))) : void; 
matching x with type: intmatching a with type: intassigning a to intmatching x with type: intassigning x to intupdating map func for  (call printint(((a: int) + (3: int): int))) : void; 
matching x with type: intupdating map func for  (call foo((40: int))) : void; 
matching a with type: intupdating map func for return (0: int); int

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

define void @"foo!3"(i32 %a) {
entry:
  %a1 = alloca i32
  store i32 %a, i32* %a1
  %a2 = load i32* %a1
  %tmp = add i32 %a2, 3
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt2, i32 0, i32 0), i32 %tmp)
  ret void
}

define i32 @main() {
entry:
  call void @"foo!3"(i32 40)
  ret i32 0
}
