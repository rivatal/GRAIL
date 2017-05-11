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
@str = private unnamed_addr constant [2 x i8] c"1\00"
@str10 = private unnamed_addr constant [16 x i8] c"49th St Station\00"
@str11 = private unnamed_addr constant [2 x i8] c"1\00"
@str12 = private unnamed_addr constant [17 x i8] c"116th St Station\00"
@str13 = private unnamed_addr constant [6 x i8] c"168th\00"

declare i32 @printf(i8*, ...)

declare i32 @sample_display(i32)

define i32 @"size!1"({ i32*, i32 } %x) {
entry:
  %x1 = alloca { i32*, i32 }
  store { i32*, i32 } %x, { i32*, i32 }* %x1
  ret i32 1
}

define void @"display!2"({ { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } } %x) {
entry:
  %x1 = alloca { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } } %x, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %x1
  ret void
}

define i32 @"size!3"({ i32*, i32 } %x) {
entry:
  %x1 = alloca { i32*, i32 }
  store { i32*, i32 } %x, { i32*, i32 }* %x1
  ret i32 1
}

define void @"display!4"({ { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } } %x) {
entry:
  %x1 = alloca { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } } %x, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %x1
  ret void
}

define void @main() {
entry:
  %strct = alloca { i32*, i32 }
  %lst = alloca i32, i32 7
  %ptr = getelementptr inbounds i32* %lst, i32 0
  store i32 0, i32* %ptr
  %ptr1 = getelementptr inbounds i32* %lst, i32 1
  store i32 1, i32* %ptr1
  %ptr2 = getelementptr inbounds i32* %lst, i32 2
  store i32 1, i32* %ptr2
  %ptr3 = getelementptr inbounds i32* %lst, i32 3
  store i32 1, i32* %ptr3
  %ptr4 = getelementptr inbounds i32* %lst, i32 4
  store i32 1, i32* %ptr4
  %ptr5 = getelementptr inbounds i32* %lst, i32 5
  store i32 1, i32* %ptr5
  %ptr6 = getelementptr inbounds i32* %lst, i32 6
  store i32 1, i32* %ptr6
  %p0 = getelementptr inbounds { i32*, i32 }* %strct, i32 0, i32 0
  %p1 = getelementptr inbounds { i32*, i32 }* %strct, i32 0, i32 1
  store i32* %lst, i32** %p0
  store i32 7, i32* %p1
  %lst7 = load { i32*, i32 }* %strct
  %0 = alloca { i32, float, i8*, float, { i32*, i32 }, i8* }
  %ptr8 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %0, i32 0, i32 0
  store i32 1500, i32* %ptr8
  %ptr9 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %0, i32 0, i32 1
  store float 0x4043F8C7E0000000, float* %ptr9
  %ptr10 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %0, i32 0, i32 2
  store i8* getelementptr inbounds ([2 x i8]* @str, i32 0, i32 0), i8** %ptr10
  %ptr11 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %0, i32 0, i32 3
  store float 0x4052CDDE60000000, float* %ptr11
  %ptr12 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %0, i32 0, i32 4
  store { i32*, i32 } %lst7, { i32*, i32 }* %ptr12
  %ptr13 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %0, i32 0, i32 5
  store i8* getelementptr inbounds ([16 x i8]* @str10, i32 0, i32 0), i8** %ptr13
  %1 = load { i32, float, i8*, float, { i32*, i32 }, i8* }* %0
  %c = alloca { i32, float, i8*, float, { i32*, i32 }, i8* }
  store { i32, float, i8*, float, { i32*, i32 }, i8* } %1, { i32, float, i8*, float, { i32*, i32 }, i8* }* %c
  %strct14 = alloca { i32*, i32 }
  %lst15 = alloca i32, i32 7
  %ptr16 = getelementptr inbounds i32* %lst15, i32 0
  store i32 0, i32* %ptr16
  %ptr17 = getelementptr inbounds i32* %lst15, i32 1
  store i32 1, i32* %ptr17
  %ptr18 = getelementptr inbounds i32* %lst15, i32 2
  store i32 1, i32* %ptr18
  %ptr19 = getelementptr inbounds i32* %lst15, i32 3
  store i32 1, i32* %ptr19
  %ptr20 = getelementptr inbounds i32* %lst15, i32 4
  store i32 1, i32* %ptr20
  %ptr21 = getelementptr inbounds i32* %lst15, i32 5
  store i32 1, i32* %ptr21
  %ptr22 = getelementptr inbounds i32* %lst15, i32 6
  store i32 0, i32* %ptr22
  %p023 = getelementptr inbounds { i32*, i32 }* %strct14, i32 0, i32 0
  %p124 = getelementptr inbounds { i32*, i32 }* %strct14, i32 0, i32 1
  store i32* %lst15, i32** %p023
  store i32 7, i32* %p124
  %lst25 = load { i32*, i32 }* %strct14
  %2 = alloca { i32, float, i8*, float, { i32*, i32 }, i8* }
  %ptr26 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %2, i32 0, i32 0
  store i32 750, i32* %ptr26
  %ptr27 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %2, i32 0, i32 1
  store float 0x4043C7AE20000000, float* %ptr27
  %ptr28 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %2, i32 0, i32 2
  store i8* getelementptr inbounds ([2 x i8]* @str11, i32 0, i32 0), i8** %ptr28
  %ptr29 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %2, i32 0, i32 3
  store float 0x4052DD2F20000000, float* %ptr29
  %ptr30 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %2, i32 0, i32 4
  store { i32*, i32 } %lst25, { i32*, i32 }* %ptr30
  %ptr31 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %2, i32 0, i32 5
  store i8* getelementptr inbounds ([17 x i8]* @str12, i32 0, i32 0), i8** %ptr31
  %3 = load { i32, float, i8*, float, { i32*, i32 }, i8* }* %2
  %d = alloca { i32, float, i8*, float, { i32*, i32 }, i8* }
  store { i32, float, i8*, float, { i32*, i32 }, i8* } %3, { i32, float, i8*, float, { i32*, i32 }, i8* }* %d
  %4 = alloca { i32 }
  %ptr32 = getelementptr inbounds { i32 }* %4, i32 0, i32 0
  store i32 1, i32* %ptr32
  %5 = load { i32 }* %4
  %g = alloca { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }
  %ptr33 = getelementptr inbounds { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 2
  store { i32 } %5, { i32 }* %ptr33
  %c34 = load { i32, float, i8*, float, { i32*, i32 }, i8* }* %c
  %c35 = load { i32, float, i8*, float, { i32*, i32 }, i8* }* %c
  %strct36 = alloca { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }
  %lst37 = alloca { i32, float, i8*, float, { i32*, i32 }, i8* }, i32 2
  %ptr38 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %lst37, i32 0
  store { i32, float, i8*, float, { i32*, i32 }, i8* } %c34, { i32, float, i8*, float, { i32*, i32 }, i8* }* %ptr38
  %ptr39 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %lst37, i32 1
  store { i32, float, i8*, float, { i32*, i32 }, i8* } %c35, { i32, float, i8*, float, { i32*, i32 }, i8* }* %ptr39
  %p040 = getelementptr inbounds { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }* %strct36, i32 0, i32 0
  %p141 = getelementptr inbounds { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }* %strct36, i32 0, i32 1
  store { i32, float, i8*, float, { i32*, i32 }, i8* }* %lst37, { i32, float, i8*, float, { i32*, i32 }, i8* }** %p040
  store i32 2, i32* %p141
  %lst42 = load { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }* %strct36
  %6 = alloca { i32 }
  %ptr43 = getelementptr inbounds { i32 }* %6, i32 0, i32 0
  store i32 1, i32* %ptr43
  %7 = load { i32 }* %6
  %8 = alloca { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }
  %ptr44 = getelementptr inbounds { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }* %8, i32 0, i32 0
  store { i32, float, i8*, float, { i32*, i32 }, i8* }* %c, { i32, float, i8*, float, { i32*, i32 }, i8* }** %ptr44
  %ptr45 = getelementptr inbounds { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }* %8, i32 0, i32 1
  store { i32, float, i8*, float, { i32*, i32 }, i8* }* %d, { i32, float, i8*, float, { i32*, i32 }, i8* }** %ptr45
  %ptr46 = getelementptr inbounds { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }* %8, i32 0, i32 2
  store i1 false, i1* %ptr46
  %ptr47 = getelementptr inbounds { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }* %8, i32 0, i32 3
  store { i32 } %7, { i32 }* %ptr47
  %9 = load { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }* %8
  %strct48 = alloca { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }
  %lst49 = alloca { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }
  %ptr50 = getelementptr inbounds { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }* %lst49, i32 0
  store { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } } %9, { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }* %ptr50
  %p051 = getelementptr inbounds { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }* %strct48, i32 0, i32 0
  %p152 = getelementptr inbounds { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }* %strct48, i32 0, i32 1
  store { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }* %lst49, { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }** %p051
  store i32 1, i32* %p152
  %lst53 = load { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }* %strct48
  %ptr54 = getelementptr inbounds { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 0
  store { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 } %lst42, { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }* %ptr54
  %ptr55 = getelementptr inbounds { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g, i32 0, i32 1
  store { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 } %lst53, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }* %ptr55
  %g56 = load { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g
  %g57 = alloca { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }
  store { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } } %g56, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g57
  %ptr58 = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %c, i32 0, i32 5
  store i8* getelementptr inbounds ([6 x i8]* @str13, i32 0, i32 0), i8** %ptr58
  %ext_val = getelementptr inbounds { i32, float, i8*, float, { i32*, i32 }, i8* }* %c, i32 0, i32 4
  %10 = load { i32*, i32 }* %ext_val
  %strct59 = alloca { i32*, i32 }
  store { i32*, i32 } %10, { i32*, i32 }* %strct59
  %tmp = getelementptr inbounds { i32*, i32 }* %strct59, i32 0, i32 1
  %len = load i32* %tmp
  %g60 = load { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } }* %g57
  %graph_display = call i32 @display({ { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } } %g60)
  ret void
}

declare i32 @display({ { { i32, float, i8*, float, { i32*, i32 }, i8* }*, i32 }, { { { i32, float, i8*, float, { i32*, i32 }, i8* }*, { i32, float, i8*, float, { i32*, i32 }, i8* }*, i1, { i32 } }*, i32 }, { i32 } })
