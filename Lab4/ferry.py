#!/usr/bin/env python

from operator import itemgetter
from clingo import Control, Model, Symbol, SymbolType
from sys import argv, flags
from pathlib import Path
from math import inf
from itertools import chain

assert len(argv) > 1, "Provide at least one filename as an argument"

program = "\n".join(Path(file).read_text() for file in argv[1:])

def get_symbol_number(sym: Symbol):
    if sym.type is SymbolType.Function:
        yield from chain.from_iterable(map(get_symbol_number, sym.arguments))
    elif sym.type is SymbolType.Number:
        yield sym.number

def symbol_index(sym: Symbol) -> int | float:
    return next(get_symbol_number(sym), -inf)

def on_model(model: Model):
    numbered = ((symbol, symbol_index(symbol)) for symbol in model.symbols(atoms=True))
    ordered = sorted(numbered, key=itemgetter(1))
    for a, _ in ordered:
        print(a)

if __name__ == "__main__" and not flags.interactive:
    ctl = Control()
    ctl.add(program)
    ctl.ground()
    ctl.solve(on_model=on_model)

    