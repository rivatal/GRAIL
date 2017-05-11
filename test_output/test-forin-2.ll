; ModuleID = 'Grail'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt1 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@str = private unnamed_addr constant [2 x i8] c"a\00"
@str2 = private unnamed_addr constant [2 x i8] c"b\00"
@str3 = private unnamed_addr constant [2 x i8] c"c\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @main() {
entry:
  %strct = alloca { i8**, i32 }
  %lst = alloca i8*, i32 3
  %ptr = getelementptr inbounds i8** %lst, i32 0
  store i8* getelementptr inbounds ([2 x i8]* @str, i32 0, i32 0), i8** %ptr
  %ptr1 = getelementptr inbounds i8** %lst, i32 1
  store i8* getelementptr inbounds ([2 x i8]* @str2, i32 0, i32 0), i8** %ptr1
  %ptr2 = getelementptr inbounds i8** %lst, i32 2
  store i8* getelementptr inbounds ([2 x i8]* @str3, i32 0, i32 0), i8** %ptr2
  %p0 = getelementptr inbounds { i8**, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { i8**, i32 }* %strct, i32 0, i32 1
  store i8** %lst, i8*** %p0
  store i32 3, i32* %p1
  %lst3 = load { i8**, i32 }* %strct
  %a = alloca { i8**, i32 }
  store { i8**, i32 } %lst3, { i8**, i32 }* %a
  %lst4 = alloca { i8**, i32 }
  %a5 = load { i8**, i32 }* %a
  store { i8**, i32 } %a5, { i8**, i32 }* %lst4
  %x = alloca i8*
  %tmp = getelementptr inbounds { i8**, i32 }* %lst4, i32 0, i32 0
  %ar = load i8*** %tmp
  %tmp6 = getelementptr inbounds { i8**, i32 }* %lst4, i32 0, i32 1
  %end = load i32* %tmp6
  %ind = alloca i32
  store i32 0, i32* %ind
  br label %while

while:                                            ; preds = %while_body, %entry
  %tmp11 = load i32* %ind
  %comp = icmp slt i32 %tmp11, %end
  br i1 %comp, label %while_body, label %merge

while_body:                                       ; preds = %while
  %i = load i32* %ind
  %ptr7 = getelementptr inbounds i8** %ar, i32 %i
  %tmp8 = load i8** %ptr7
  store i8* %tmp8, i8** %x
  %x9 = load i8** %x
  %printf = call i32 (i8*, ...)* @printf(i8* %x9)
  %tmp10 = load i32* %ind
  %inc = add i32 %tmp10, 1
  store i32 %inc, i32* %ind
  br label %while

merge:                                            ; preds = %while
  ret i32 0
}
