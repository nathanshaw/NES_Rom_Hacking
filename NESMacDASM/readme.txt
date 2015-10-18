NESMacDASM v1.0   pairofrooks@gmail.com   Jul 12 2009
(based on Tennessee Carmel-Veilleux's DCC6502)

It's hard to find good tools for the Mac; increasingly so for us with PPC Macs.  I ported/rewrote/duct-taped this disassembler for .NES ROMs over the course of a weekend, merely because it was raining. I did add a handy function-identification feature I haven't seen in other disassemblers. Whether I further improve the feature, or the tool as a whole, depends on user interest.

It is a command-line tool ran from Mac OS X's Terminal app, of course. It was tested on 10.4 "Tiger" on a G4 eMac.  

If it's a universal binary, I don't know of it. Try it out.

Four files are contained in the .zip: 
readme.txt:		the ubiquitous readme
NESMacDASM:		the program itself
NESMacDASM.c:		the complete source code for it (I like small)
Balloon Fight.txt:	an example of its work

Command-line options are concise:
        -h      -> add standard hex bytes to disassembly
        -c      -> add cycle-counting info

Examples of use: 
        NESMacDASM zelda.nes
        NESMacDASM -h zelda.nes > zelda.txt


FEATURES

I have included a sample disassembly of _Balloon Fight_ partly to highlight the features of NESMacDASM, and partly so non-PPC-Mac users may understand what I speak of without having to hunt down a PPC Mac.

I added a feature I haven't seen in other disassemblers.  NESMacDASM can identify where functions begin and end with about 95% accuracy.  It will mark these boundaries with a blank line.  Functions frequently end at an RTS, RTI, or JMP, unless said opcode can be skipped over by a previous branch instruction.  A comment is instead added in this case.  See the two RTS opcodes in the function at $C461 for an example.

Occasionally, a function will be given an auto-generated name as well if the disassembler is pretty sure about it.  Some "functions" however are reached only via a JMP from a zero-page function, which can only be checked for at runtime. Likewise jump tables -- the included example has a jump table at $C3A8, clearly showing (to the human observer) that $C3BF, $C334, etc., are "function" entry points, but NESMacDASM can't (yet) understand this. 

Occasionally, embedded data will cause a opcode/operand misalignment, but even then NESMacDASM will sometimes purposefully correct itself. An example of correction can be seen at $C770, wherein the first opcode of the function was actually already eaten by the preceding ORA -- which is actually data, not code. 

False positives rarely occur.  But an example of such a false function start point is at $C804 -- the function actually began many bytes back at $C786, and this new entry point is spurious.  Fortunately, this happens so infrequently that the feature is still very much worth it. 

NESMacDASM has some auto-commenting features seen in other disassemblers, such as when we access hardware I/O ports or save-RAM.  Nothing special there.  The three interrupt vectors are listed in the output file's header for convenience.  


WISHLIST

There are some features that could still be added.  

A symbol table (as an input file) could allow us to name functions, to define locations as data, code, entry/exit point, or not-a-entry/exit point.  Many of the errors that slip by NESMacDASM are specifically caused by the inability to recognize bits of data sprinkled among the code. (Any long run of unknown opcodes is certainly data.)

Auto-generated callgraph. This output would likely need be a separate HTML output file for readability.  Reconstructing the gameplay directly from the code becomes feasible with a callgraph. Think of it as the 10,000 foot view.

Pseudo-emulation during disassembly.  Keeping loose track of the state of the processor flags can improve the final output. Cases where a LDA #$00 is followed by a BEQ do occur, and NESMacDASM cannot currently recognize that the BEQ in that instance is an unconditional jump, potentially ending the function.  Likewise the afore-mentioned JMP tables would benefit from pseudo-emulation.

A second pass would allow the beginning of the ROM to have as much knowledge applied to it as the latter parts of the ROM. An auto-generated symbol table could help, though it would require running the tool twice for full effect. 

Mappers: I still don't understand them, and the 16-byte header at the beginning of a .NES ROM I'm still not sure if it's completely accurate.



Thoughts on NESMacDASM are welcome.

Thank you, 
 --Rook
