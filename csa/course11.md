Chapter 5. Interrupts


Interrupt Vector Table - loaded at OS loading time, in the first 1024 bytes of RAM.
IHR - Interrupt Handling Routine

Addr(IHR(0)) - at 0000:0000
Addr(IHR(1)) - at 0000:0004
Addr(IHR(2)) - at 0000:0008
---------------------------
Addr(IHR(k)) - at 0000:(k*4)
^ specific to MS-DOS and Windows. *NIX provides a list of callable functions instead of a table of IHR addresses (which can be modified).

Each Addr(IHR()) points to a function for some interrupt (handler).


Types of Interrupt Handling Routine:
    - written by the vendor of the CS
    - written by the designers of the OS
    - written by the user

Types of interrupts:
    - hardware interrupts (=BIOS interrupts)
        - automatically generated, as a reaction to external causes
        - the routines are loaded in memory from the ROM-BIOS files at CS startup)
        - the program ALWAYS resumes

    - exceptions
        - automatically generated, as a result of some internal causes
            zero division
            attempt of running an inexistent instruction
            accessing restricted memory area))
        - the program DOES NOT usually resume

    - software interrupts (=traps)
        - never automatically generated
        - the programmer initiate execution control transfer to the corresponding IHR (handler) by using the INT instruction
        - are called software interrupts because they are SOFTWARE invoked by an explicit instruction

    All interrupts can be initiated by the programmer by the INT instruction.
    Those interrupts that can ONLY be initiated by the programmer by INT are software interrupts.

