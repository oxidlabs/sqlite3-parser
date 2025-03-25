#![allow(dead_code)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt<'a> {
    /// `ALTER TABLE`: table name, body
    AlterTable(AlterTableStmt<'a>),
    /// `ANALYSE`: object name
    Analyze(Option<Analyze<'a>>),
    /// `ATTACH DATABASE`
    Attach(AttachStmt<'a>),
    /// `BEGIN`: tx type, tx name
    Begin(Option<TransactionType>, Option<&'a str>),
    /// `COMMIT`/`END`: tx commit/end, tx name
    Commit(bool, Option<&'a str>),
    /// `CREATE INDEX`
    CreateIndex {
        unique: bool,
        if_not_exists: bool,
        schema_name: Option<&'a str>,
        index_name: &'a str,
        table_name: &'a str,
        columns: Vec<IndexedColumn<'a>>,
        where_clause: Option<Box<Expr<'a>>>,
    },
    /// `CREATE TABLE`
    CreateTable{
        temp_temporary: Option<TempTemporary>,
        if_not_exists: bool,
        schema_name: Option<&'a str>,
        table_name: &'a str,
        body: Box<CreateTableBody<'a>>,
    },
    /// `CREATE TRIGGER`
    CreateTrigger{
        temp_temporary: Option<TempTemporary>,
        if_not_exists: bool,
        schema_name: Option<&'a str>,
        trigger_name: &'a str,
        time: Option<TriggerTime>,
        event: TriggerEvent<'a>,
        on_table: &'a str,
        for_each_row: bool,
        when_expr: Option<Box<Expr<'a>>>,
        trigger_body: Vec<Stmt<'a>>,
    },
    /// `CREATE VIEW`
    CreateView{
        temp_temporary: Option<TempTemporary>,
        if_not_exists: bool,
        schema_name: Option<&'a str>,
        view_name: &'a str,
        column_names: Option<Vec<&'a str>>,
        select_stmt: Box<SelectStmt<'a>>,
    },
    /// `CREATE VIRTUAL TABLE`
    CreateVirtualTable{
        if_not_exists: bool,
        schema_name: Option<&'a str>,
        table_name: &'a str,
        module_name: &'a str,
        module_args: Vec<&'a str>,
    },
    /// `DELETE`
    Delete{
        with_clause: Option<WithClause<'a>>,
        qualified_table_name: QualifiedTableName<'a>,
        where_clause: Option<Box<Expr<'a>>>,
        returning_clause: Option<ReturningClause<'a>>,
        order_by_clause: Option<OrderByClause<'a>>,
        limit_clause: Option<LimitClause<'a>>,
    },
    /// `DETACH DATABASE`: db name
    Detach(Option<&'a str>),
    /// `DROP INDEX`
    DropIndex{
        if_exists: bool,
        schema_name: Option<&'a str>,
        index_name: &'a str,
    },
    /// `DROP TABLE`
    DropTable{
        if_exists: bool,
        schema_name: Option<&'a str>,
        table_name: &'a str,
    },
    /// `DROP TRIGGER`
    DropTrigger{
        if_exists: bool,
        schema_name: Option<&'a str>,
        trigger_name: &'a str,
    },
    /// `DROP VIEW`
    DropView{
        if_exists: bool,
        schema_name: Option<&'a str>,
        view_name: &'a str,
    },
    /// `INSERT`
    Insert{
        with_clause: Option<WithClause<'a>>,
        or_conflict: Option<ConflictClause>,
        schema_name: Option<&'a str>,
        table_name: &'a str,
        alias: Option<&'a str>,
        column_names: Option<Vec<&'a str>>,
        data_source: InsertDataSource<'a>,
        returning_clause: Option<ReturningClause<'a>>,
    },
    /// `PRAGMA`: pragma name, body
    Pragma{
        schema_name: Option<&'a str>,
        pragma_name: &'a str,
        pragma_value: Option<PragmaValue<'a>>,
    },
    /// `REINDEX`
    Reindex{
        collation_name: Option<&'a str>,
        index_name: Option<&'a str>,
        schema_table_name: Option<SchemaTableName<'a>>,
    },
    /// `RELEASE`: savepoint name
    Release(&'a str),
    /// `ROLLBACK`
    Rollback{
        savepoint_name: Option<&'a str>,
    },
    /// `SAVEPOINT`: savepoint name
    Savepoint(&'a str),
    /// `SELECT`
    Select(Box<SelectStmt<'a>>),
    /// `UPDATE`
    Update{
        with_clause: Option<WithClause<'a>>,
        or_conflict: Option<ConflictClause>,
        qualified_table_name: QualifiedTableName<'a>,
        set_clauses: Vec<SetClause<'a>>,
        from_clause: Option<FromClause<'a>>,
        where_clause: Option<Box<Expr<'a>>>,
        returning_clause: Option<ReturningClause<'a>>,
        order_by_clause: Option<OrderByClause<'a>>,
        limit_clause: Option<LimitClause<'a>>,
    },
    /// `VACUUM`: database name, into expr
    Vacuum{
        schema_name: Option<&'a str>,
        into_file: Option<Box<Expr<'a>>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TempTemporary {
    Temp,
    Temporary,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CreateTableBody<'a> {
    ColumnsAndConstraints {
        columns: Vec<ColumnDef<'a>>,
        constraints: Vec<TableConstraint<'a>>,
    },
    AsSelect(Box<SelectStmt<'a>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexedColumn<'a> {
    pub column_name: &'a str,
    pub expr: Option<Box<Expr<'a>>>,
    pub collation: Option<&'a str>,
    pub order: Option<ColumnOrder>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ColumnOrder {
    Ascending,
    Descending,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr<'a> {
    Literal(Literal<'a>),
    BindParameter(&'a str),
    Column {
        // if there is a schema_name there is a table_name
        // there is also a dot inbetween schema and table
        schema_name: Option<&'a str>,
        table_name: Option<&'a str>,
        column_name: &'a str,
    },
    UnaryOp {
        op: UnaryOperator,
        expr: Box<Expr<'a>>,
    },
    BinaryOp {
        left: Box<Expr<'a>>,
        op: BinaryOperator,
        right: Box<Expr<'a>>,
    },
    FunctionCall {
        name: &'a str,
        args: Vec<Expr<'a>>,
        filter_clause: Option<Box<Expr<'a>>>,
        over_clause: Option<Box<Expr<'a>>>,
    },
    Cast {
        expr: Box<Expr<'a>>,
        type_name: &'a str,
    },
    Collate {
        expr: Box<Expr<'a>>,
        collation: &'a str,
    },
    Like {
        expr: Box<Expr<'a>>,
        pattern: Box<Expr<'a>>,
        escape: Option<Box<Expr<'a>>>,
    },
    Glob {
        expr: Box<Expr<'a>>,
        pattern: Box<Expr<'a>>,
    },
    Regexp {
        expr: Box<Expr<'a>>,
        pattern: Box<Expr<'a>>,
    },
    Match {
        expr: Box<Expr<'a>>,
        pattern: Box<Expr<'a>>,
    },
    IsNull(Box<Expr<'a>>),
    NotNull(Box<Expr<'a>>),
    Is {
        expr: Box<Expr<'a>>,
        not: bool,
        right: Box<Expr<'a>>,
    },
    Between {
        expr: Box<Expr<'a>>,
        not: bool,
        low: Box<Expr<'a>>,
        high: Box<Expr<'a>>,
    },
    InList {
        expr: Box<Expr<'a>>,
        not: bool,
        list: Vec<Expr<'a>>,
    },
    InSelect {
        expr: Box<Expr<'a>>,
        not: bool,
        select: Box<SelectStmt<'a>>,
    },
    InTable {
        expr: Box<Expr<'a>>,
        not: bool,
        schema: Option<&'a str>,
        table: &'a str,
    },
    Exists(Box<SelectStmt<'a>>),
    Case {
        expr: Option<Box<Expr<'a>>>,
        when_then: Vec<(Expr<'a>, Expr<'a>)>,
        else_expr: Option<Box<Expr<'a>>>,
    },
    RaiseFunction(RaiseFunction<'a>),
    Subquery(Box<SelectStmt<'a>>),
    WindowFunction {
        expr: Box<Expr<'a>>,
        partition_by: Vec<Expr<'a>>,
        order_by: Option<OrderByClause<'a>>,
        frame_spec: Option<FrameSpec>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RaiseFunction<'a> {
    Ignore,
    Rollback(Box<Expr<'a>>),
    Abort(Box<Expr<'a>>),
    Fail(Box<Expr<'a>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Literal<'a> {
    /// Number
    Numeric(&'a str),
    /// String
    String(&'a str),
    /// BLOB
    Blob(&'a str),
    /// Keyword
    Keyword(&'a str),
    /// `NULL`
    Null,
    /// `CURRENT_DATE`
    CurrentDate,
    /// `CURRENT_TIME`
    CurrentTime,
    /// `CURRENT_TIMESTAMP`
    CurrentTimestamp,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
    Negative,   // '-'
    Positive,   // '+'
    BitwiseNot, // '~'
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

// https://sqlite.org/syntax/alter-table-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AlterTable<'a> {
    RenameTable(&'a str),
    RenameColumn {
        column_name: &'a str,
        new_name: &'a str,
    },
    Add(&'a str),
    Drop(&'a str),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AlterTableStmt<'a> {
    pub schema_name: Option<&'a str>,
    pub table_name: &'a str,
    pub stmt: AlterTable<'a>,
}

// https://sqlite.org/syntax/analyze-stmt.html
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Analyze<'a> {
    SchemaName(&'a str),
    TableName(&'a str),
    SchemaDotTableName {
        schema_name: &'a str,
        table_name: &'a str,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AttachStmt<'a> {
    pub schema_name: &'a str,
    pub expr: Box<Expr<'a>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TransactionType {
    /// `DEFERRED`
    Deferred,
    /// `IMMEDIATE`
    Immediate,
    /// `EXCLUSIVE`
    Exclusive,
}

// New structures to support the completed AST

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SelectStmt<'a> {
    pub with_clause: Option<WithClause<'a>>,
    pub compound_operator: Option<CompoundOperator>,
    pub select_core: SelectCore<'a>,
    pub order_by_clause: Option<OrderByClause<'a>>,
    pub limit_clause: Option<LimitClause<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SelectCore<'a> {
    pub distinct: bool,
    pub result_columns: Vec<ResultColumn<'a>>,
    pub from_clause: Option<FromClause<'a>>,
    pub where_clause: Option<Box<Expr<'a>>>,
    pub group_by_clause: Option<GroupByClause<'a>>,
    pub having_clause: Option<Box<Expr<'a>>>,
    pub window_clause: Option<WindowClause<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ResultColumn<'a> {
    AllColumns,
    TableAllColumns(&'a str),
    Expr {
        expr: Box<Expr<'a>>,
        alias: Option<&'a str>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FromClause<'a> {
    pub tables: Vec<TableOrSubquery<'a>>,
    pub join_clauses: Vec<JoinClause<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TableOrSubquery<'a> {
    Table {
        schema_name: Option<&'a str>,
        table_name: &'a str,
        alias: Option<&'a str>,
        indexed_by: Option<&'a str>,
        not_indexed: bool,
    },
    Subquery {
        select_stmt: Box<SelectStmt<'a>>,
        alias: Option<&'a str>,
    },
    TableFunction {
        name: &'a str,
        args: Vec<Expr<'a>>,
        alias: Option<&'a str>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct JoinClause<'a> {
    pub join_type: JoinType,
    pub table_or_subquery: TableOrSubquery<'a>,
    pub constraint: JoinConstraint<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JoinType {
    Inner,
    Left,
    LeftOuter,
    Cross,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JoinConstraint<'a> {
    On(Box<Expr<'a>>),
    Using(Vec<&'a str>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GroupByClause<'a> {
    pub exprs: Vec<Expr<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WindowClause<'a> {
    pub window_defs: Vec<(WindowName<'a>, WindowDef<'a>)>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WindowName<'a> {
    Name(&'a str),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WindowDef<'a> {
    pub base_window_name: Option<&'a str>,
    pub partition_by: Vec<Expr<'a>>,
    pub order_by: Option<OrderByClause<'a>>,
    pub frame_spec: Option<FrameSpec>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FrameSpec {
    pub type_: FrameType,
    pub bounds: FrameBounds,
    pub exclude: Option<FrameExclude>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FrameType {
    Range,
    Rows,
    Groups,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FrameBounds {
    Between(FrameBound, FrameBound),
    Start(FrameBound),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FrameBound {
    Unbounded,
    Current,
    Offset(i64),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FrameExclude {
    NoOthers,
    CurrentRow,
    Group,
    Ties,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WithClause<'a> {
    pub recursive: bool,
    pub cte_tables: Vec<CommonTableExpression<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CommonTableExpression<'a> {
    pub table_name: &'a str,
    pub column_names: Option<Vec<&'a str>>,
    pub select_stmt: Box<SelectStmt<'a>>,
    pub materialized: Option<bool>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OrderByClause<'a> {
    pub terms: Vec<OrderingTerm<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OrderingTerm<'a> {
    pub expr: Box<Expr<'a>>,
    pub asc_desc: Option<AscDesc>,
    pub nulls: Option<NullsOrder>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AscDesc {
    Asc,
    Desc,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NullsOrder {
    First,
    Last,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LimitClause<'a> {
    pub limit: Box<Expr<'a>>,
    pub offset: Option<Box<Expr<'a>>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompoundOperator {
    Union,
    UnionAll,
    Intersect,
    Except,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColumnDef<'a> {
    pub name: &'a str,
    pub type_name: Option<&'a str>,
    pub constraints: Vec<ColumnConstraint<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ColumnConstraint<'a> {
    PrimaryKey {
        order: Option<ColumnOrder>,
        conflict_clause: Option<ConflictClause>,
        autoincrement: bool,
    },
    NotNull(Option<ConflictClause>),
    Unique(Option<ConflictClause>),
    Check(Box<Expr<'a>>),
    Default(DefaultValue<'a>),
    Collate(&'a str),
    ForeignKey(ForeignKeyClause<'a>),
    Generated {
        expr: Box<Expr<'a>>,
        storage: Option<GeneratedColumnStorage>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GeneratedColumnStorage {
    Stored,
    Virtual,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DefaultValue<'a> {
    Expr(Box<Expr<'a>>),
    LiteralValue(Literal<'a>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ForeignKeyClause<'a> {
    pub table_name: &'a str,
    pub column_names: Option<Vec<&'a str>>,
    pub actions: Vec<ForeignKeyAction>,
    pub deferrable: Option<Deferrable>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForeignKeyAction {
    OnDelete(ForeignKeyActionType),
    OnUpdate(ForeignKeyActionType),
    OnInsert(ForeignKeyActionType),
    Match(&'static str),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForeignKeyActionType {
    SetNull,
    SetDefault,
    Cascade,
    Restrict,
    NoAction,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Deferrable {
    NotDeferrable,
    Deferrable(Option<InitiallyDeferred>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InitiallyDeferred {
    InitiallyDeferred,
    InitiallyImmediate,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TableConstraint<'a> {
    PrimaryKey {
        name: Option<&'a str>,
        columns: Vec<IndexedColumn<'a>>,
        conflict_clause: Option<ConflictClause>,
    },
    Unique {
        name: Option<&'a str>,
        columns: Vec<IndexedColumn<'a>>,
        conflict_clause: Option<ConflictClause>,
    },
    Check {
        name: Option<&'a str>,
        expr: Box<Expr<'a>>,
    },
    ForeignKey {
        name: Option<&'a str>,
        columns: Vec<&'a str>,
        foreign_key_clause: ForeignKeyClause<'a>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConflictClause {
    Rollback,
    Abort,
    Fail,
    Ignore,
    Replace,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TriggerTime {
    Before,
    After,
    InsteadOf,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TriggerEvent<'a> {
    Delete,
    Insert,
    Update(Option<Vec<&'a str>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QualifiedTableName<'a> {
    pub schema_name: Option<&'a str>,
    pub table_name: &'a str,
    pub alias: Option<&'a str>,
    pub indexed_by: Option<&'a str>,
    pub not_indexed: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReturningClause<'a> {
    pub columns: Vec<ResultColumn<'a>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PragmaValue<'a> {
    Equals(Box<Expr<'a>>),
    Parenthesized(Box<Expr<'a>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SchemaTableName<'a> {
    pub schema_name: Option<&'a str>,
    pub table_name: &'a str,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum InsertDataSource<'a> {
    Values(Vec<Vec<Expr<'a>>>),
    Select(Box<SelectStmt<'a>>),
    DefaultValues,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SetClause<'a> {
    pub column_name: &'a str,
    pub expr: Box<Expr<'a>>,
}
