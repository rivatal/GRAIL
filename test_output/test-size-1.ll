updating map func for (a: list of int) = ((1: int)(2: int)(3: int) : list of int); 
matching x with type: list of intassigning x to list of intupdating map func for return (1: int); int

updating map func for (b: int) = (call size((a: list of int))) : int; 
matching x with type: list of intmatching x with type: intassigning x to intupdating map func for  (call printint((b: int))) : void; 
matching x with type: intupdating map func for return (0: int); int

; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt3 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmt4 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt5 = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @"size!1"({ i32*, i32 } %x) {
entry:
  %x1 = alloca { i32*, i32 }
  store { i32*, i32 } %x, { i32*, i32 }* %x1
  ret i32 1
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
  %a4 = load { i32*, i32 }* %a
  %strct5 = alloca { i32*, i32 }
  store { i32*, i32 } %a4, { i32*, i32 }* %strct5
  %tmp = getelementptr inbounds { i32*, i32 }* %strct5, i32 0, i32 1
  %len = load i32* %tmp
  %b = alloca i32
  store i32 %len, i32* %b
  %b6 = load i32* %b
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmt4, i32 0, i32 0), i32 %b6)
  ret i32 0
}
