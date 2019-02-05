# Formatters-That-Work
Trees-That-Grow Haskell 

# src/

- TtgAST.hs -- определение расширяемого AST для подмножества Haskell.
- CommonTtgAST.hs -- расширение AST, которое сохраняет метаинформацию о символах языка (положение и аннотации)
- SrcInfo.hs -- определение типа для хранения этой информации.
- CommonASTParser.hs -- непосредственно "парсер": берёт AST с аннотациями у Exact-Print и переводит в наше дерево.
- SimpleTtgAST.hs -- расширение AST, которое ничего не расширяет.
- SimpleConverter.hs -- экспортирует функцию, которая из любого расширяемого дерева даст простое дерево без расширений.
- OneLineNoCommentsPrinter.hs -- строит строку из SimpleTtgAST для печати кода в одну строку без комментариев.
- AnnTtgAST.hs -- расширение AST, которое сохраняет аннотации для вывода кода с комментариями.
- AnnConverter.hs -- преобразование CommonTtgAST в AnnTtgAST.
- OneLinePrinter.hs -- строит строку для печати кода с комментариями в одну строку (WIP).


# Build'n'Run

Собрать исполняемый файл: `stack build`.

Запустить: `stack exec ftw-exe file.hs`.
