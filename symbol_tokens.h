
#ifndef SYMBOL_TOKENS_H
#define SYMBOL_TOKENS_H

// If in doubt, you probably want oh_symbolToken rather than this.  This is
// just a means by which a symbol table and a heap cooperate to create a unique
// object ("token") for each symbol.
//
FUNC Object ob_createToken( Symbol sy, ObjectHeap heap );

#endif

