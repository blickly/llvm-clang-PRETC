; RUN: llvm-as < %s | llc -march=msp430

define i16 @test(double %d) nounwind {
entry:
        %add = add double %d, 1.000000e+00
        %call = tail call i16 @funct(double %add) nounwind
        ret i16 %call
}

declare i16 @funct(double)

