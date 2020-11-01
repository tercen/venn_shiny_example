# Venn diagram operator

##### Description

The `venn_shiny_operator` creates a Venn diagram.

##### Usage

Input projection|.
---|---
`column_1`        | first column track containing categories to compare 
`column_2`        | second column track containing categories to compare
`...`        | ...
`column_n`        | last column, up to 5

Output relations|.
---|---
`Operator view`        | view of the Venn diagram

##### Details

The operator is based on the `venn.diagram` function of the [VennDiagram R package](https://www.rdocumentation.org/packages/VennDiagram/versions/1.6.20/topics/venn.diagram).

See the [Venn diagram page on Wikipedia](https://en.wikipedia.org/wiki/Venn_diagram).

