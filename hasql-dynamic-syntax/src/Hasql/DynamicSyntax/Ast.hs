{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is a modified version of PostgresqlSyntax.Ast which carries values to apply to parameters and decoders for
-- results.
module Hasql.DynamicSyntax.Ast where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.IP (IPRange)
import Data.Int (Int16, Int32, Int64)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific (Scientific)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import qualified Hasql.DynamicSyntax.Ast.Internal as Internal
import Network.IP.Addr (IP, NetAddr)
import PostgresqlSyntax.Ast (
  AliasClause,
  AnyName,
  AscDesc,
  Attrs,
  Bconst,
  Character,
  Class,
  ColId,
  Collate,
  CollateClause,
  ConstCharacter,
  ConstDatetime,
  CursorName,
  ExistingWindowName,
  ExtractArg,
  Fconst,
  ForLockingStrength,
  FrameClauseMode,
  Iconst,
  Ident (..),
  Interval,
  JoinType,
  Name,
  NullsOrder,
  OptOrdinality,
  OptVarying,
  OverrideKind,
  QualAllOp,
  QualOp,
  Sconst,
  SelectBinOp,
  SubType,
  SubqueryOp,
  SymbolicExprBinOp,
  TrimModifier,
  TypeFunctionName,
  TypenameArrayDimensions,
  VerbalExprBinOp,
  WindowExclusionClause,
  Xconst,
 )

-- | A kind-level type flagging a column as nullable.
data Null

-- | A kind-level type flagging a column as not nullable.
data NotNull

-- | A singleton GADT that carries type-level nullability information.
data Nullability a where
  -- | Indicates that a column can be null
  Null :: Nullability Null
  -- | Indicates that a column cannot be null
  NotNull :: Nullability NotNull

-- | A typeclass used to build statically known @Nullability@ values from type information.
class SingNullability a where singNullability :: Nullability a

instance SingNullability Null where singNullability = Null
instance SingNullability NotNull where singNullability = NotNull

deriving instance Show (Nullability a)
deriving instance Eq (Nullability a)
deriving instance Ord (Nullability a)

data SqlBool
data SqlInt2
data SqlInt4
data SqlInt8
data SqlFloat4
data SqlFloat8
data SqlNumeric
data SqlChar
data SqlText
data SqlBytea
data SqlDate
data SqlTimestamp
data SqlTimestampz
data SqlTime
data SqlTimez
data SqlInterval
data SqlUUID
data SqlInet
data SqlJson
data SqlJsonb
data SqlArray t

-- | A singleton GADT that carries type-level column type information.
data SqlType t where
  SqlBool :: SqlType SqlBool
  SqlInt2 :: SqlType SqlInt2
  SqlInt4 :: SqlType SqlInt4
  SqlInt8 :: SqlType SqlInt8
  SqlFloat4 :: SqlType SqlFloat4
  SqlFloat8 :: SqlType SqlFloat8
  SqlNumeric :: SqlType SqlNumeric
  SqlChar :: SqlType SqlChar
  SqlText :: SqlType SqlText
  SqlBytea :: SqlType SqlBytea
  SqlDate :: SqlType SqlDate
  SqlTimestamp :: SqlType SqlTimestamp
  SqlTimestampz :: SqlType SqlTimestampz
  SqlTime :: SqlType SqlTime
  SqlTimez :: SqlType SqlTimez
  SqlInterval :: SqlType SqlInterval
  SqlUUID :: SqlType SqlUUID
  SqlInet :: SqlType SqlInet
  SqlJson :: SqlType SqlJson
  SqlJsonb :: SqlType SqlJsonb
  SqlArray :: !(ColumnType t) -> SqlType (SqlArray (ColumnType t))

-- | A typeclass used to build statically known @SqlType@ values from type information.
class SingSqlType a where singSqlType :: SqlType a

instance SingSqlType SqlBool where singSqlType = SqlBool
instance SingSqlType SqlInt2 where singSqlType = SqlInt2
instance SingSqlType SqlInt4 where singSqlType = SqlInt4
instance SingSqlType SqlInt8 where singSqlType = SqlInt8
instance SingSqlType SqlFloat4 where singSqlType = SqlFloat4
instance SingSqlType SqlFloat8 where singSqlType = SqlFloat8
instance SingSqlType SqlNumeric where singSqlType = SqlNumeric
instance SingSqlType SqlChar where singSqlType = SqlChar
instance SingSqlType SqlText where singSqlType = SqlText
instance SingSqlType SqlBytea where singSqlType = SqlBytea
instance SingSqlType SqlDate where singSqlType = SqlDate
instance SingSqlType SqlTimestamp where singSqlType = SqlTimestamp
instance SingSqlType SqlTimestampz where singSqlType = SqlTimestampz
instance SingSqlType SqlTime where singSqlType = SqlTime
instance SingSqlType SqlTimez where singSqlType = SqlTimez
instance SingSqlType SqlInterval where singSqlType = SqlInterval
instance SingSqlType SqlUUID where singSqlType = SqlUUID
instance SingSqlType SqlInet where singSqlType = SqlInet
instance SingSqlType SqlJson where singSqlType = SqlJson
instance SingSqlType SqlJsonb where singSqlType = SqlJsonb
instance (SingColumnType t nullability) => SingSqlType (SqlArray (ColumnType '(t, nullability))) where
  singSqlType = SqlArray singColumnType

deriving instance Show (SqlType t)
deriving instance Eq (SqlType t)
deriving instance Ord (SqlType t)

-- | A singleton GADT that carries type-level column type and nullability information.
data ColumnType t where
  ColumnType :: !(SqlType t) -> !(Nullability nullable) -> ColumnType '(t, nullable)

-- | A constraint used to build statically known @ColumnType@ values from type information.
type SingColumnType t nullability = (SingSqlType t, SingNullability nullability)

-- | Build the @ColumnType@ singleton for statically-known type and nullability values.
singColumnType :: (SingColumnType t nullability) => ColumnType '(t, nullability)
singColumnType = ColumnType singSqlType singNullability

deriving instance Show (ColumnType t)
deriving instance Eq (ColumnType t)
deriving instance Ord (ColumnType t)

-- | Represents the rows that a single target element would produce. E.g. column targets produce one row and wildcard
-- (*) targets produce multiple.
data TargetTypes row where
  TargetTypesNil :: TargetTypes '[]
  TargetTypesCons :: ColumnType t -> TargetTypes row -> TargetTypes (t ': row)

class SingTargetTypes row where singTargetTypes :: TargetTypes row
instance SingTargetTypes '[] where singTargetTypes = TargetTypesNil
instance
  (SingColumnType t nullability, SingTargetTypes row)
  => SingTargetTypes ('(t, nullability) ': row)
  where
  singTargetTypes = TargetTypesCons singColumnType singTargetTypes

deriving instance Show (TargetTypes row)
deriving instance Eq (TargetTypes row)
deriving instance Ord (TargetTypes row)

-- | Represents the nested list of rows that a target list produce.
data TargetTypesList rows where
  TargetTypesListNil :: TargetTypesList '[]
  TargetTypesListCons :: TargetTypes (r ': row) -> TargetTypesList rows -> TargetTypesList ((r ': row) ': rows)

deriving instance Show (TargetTypesList rows)
deriving instance Eq (TargetTypesList rows)
deriving instance Ord (TargetTypesList rows)

-- | Relates the known arguments of @SqlType@ to a corresponding Haskell value type.
type family SqlToHask (t :: Type) :: Type where
  SqlToHask SqlBool = Bool
  SqlToHask SqlInt2 = Int16
  SqlToHask SqlInt4 = Int32
  SqlToHask SqlInt8 = Int64
  SqlToHask SqlFloat4 = Float
  SqlToHask SqlFloat8 = Double
  SqlToHask SqlNumeric = Scientific
  SqlToHask SqlChar = Char
  SqlToHask SqlText = Text
  SqlToHask SqlBytea = ByteString
  SqlToHask SqlDate = Day
  SqlToHask SqlTimestamp = LocalTime
  SqlToHask SqlTimestampz = UTCTime
  SqlToHask SqlTime = TimeOfDay
  SqlToHask SqlTimez = (TimeOfDay, TimeZone)
  SqlToHask SqlInterval = DiffTime
  SqlToHask SqlUUID = UUID
  SqlToHask SqlInet = IPRange
  SqlToHask SqlJson = Value
  SqlToHask SqlJsonb = Value
  SqlToHask (SqlArray (ColumnType t)) = Vector (ColumnToHask t)

-- | Relates the type and nullability info carried by a @ColumnType` to a Haskell value type.
type family ColumnToHask (t :: (Type, Type)) :: Type where
  ColumnToHask '(t, Null) = Maybe (SqlToHask t)
  ColumnToHask '(t, NotNull) = (SqlToHask t)

-- * Statement

-- |
-- ==== References
-- @
-- PreparableStmt:
--   |  SelectStmt
--   |  InsertStmt
--   |  UpdateStmt
--   |  DeleteStmt
--   |  CallStmt
-- @
data PreparableStmt (rows :: [[(Type, Type)]])
  = SelectPreparableStmt (SelectStmt rows)
  | InsertPreparableStmt (InsertStmt rows)
  | UpdatePreparableStmt (UpdateStmt rows)
  | DeletePreparableStmt (DeleteStmt rows)
  | CallPreparableStmt (CallStmt rows)

simpleSelectPreparableStmt :: SimpleSelect rows -> PreparableStmt rows
simpleSelectPreparableStmt select =
  SelectPreparableStmt $ Left $ SelectNoParens Nothing (Left select) Nothing Nothing Nothing

-- * Call

data CallStmt rows
  = CallStmt (TargetTypesList rows) FuncApplication

-- * Insert

-- |
-- ==== References
-- @
-- InsertStmt:
--   | opt_with_clause INSERT INTO insert_target insert_rest
--       opt_on_conflict returning_clause
-- @
data InsertStmt rows = InsertStmt (Maybe WithClause) InsertTarget InsertRest (Maybe OnConflict) (ReturningClause rows)

-- |
-- ==== References
-- @
-- insert_target:
--   | qualified_name
--   | qualified_name AS ColId
-- @
data InsertTarget = InsertTarget QualifiedName (Maybe ColId)

-- |
-- ==== References
-- @
-- insert_rest:
--   | SelectStmt
--   | OVERRIDING override_kind VALUE_P SelectStmt
--   | '(' insert_column_list ')' SelectStmt
--   | '(' insert_column_list ')' OVERRIDING override_kind VALUE_P SelectStmt
--   | DEFAULT VALUES
-- @
data InsertRest
  = forall rows. SelectInsertRest (Maybe InsertColumnList) (Maybe OverrideKind) (SelectStmt rows)
  | DefaultValuesInsertRest

-- |
-- ==== References
-- @
-- insert_column_list:
--   | insert_column_item
--   | insert_column_list ',' insert_column_item
-- @
type InsertColumnList = NonEmpty InsertColumnItem

-- |
-- ==== References
-- @
-- insert_column_item:
--   | ColId opt_indirection
-- @
data InsertColumnItem = InsertColumnItem ColId (Maybe Indirection)

-- |
-- ==== References
-- @
-- opt_on_conflict:
--   | ON CONFLICT opt_conf_expr DO UPDATE SET set_clause_list where_clause
--   | ON CONFLICT opt_conf_expr DO NOTHING
--   | EMPTY
-- @
data OnConflict = OnConflict (Maybe ConfExpr) OnConflictDo

-- |
-- ==== References
-- @
-- opt_on_conflict:
--   | ON CONFLICT opt_conf_expr DO UPDATE SET set_clause_list where_clause
--   | ON CONFLICT opt_conf_expr DO NOTHING
--   | EMPTY
-- @
data OnConflictDo
  = UpdateOnConflictDo SetClauseList (Maybe AExpr)
  | NothingOnConflictDo

-- |
-- ==== References
-- @
-- opt_conf_expr:
--   | '(' index_params ')' where_clause
--   | ON CONSTRAINT name
--   | EMPTY
-- @
data ConfExpr
  = WhereConfExpr IndexParams (Maybe AExpr)
  | ConstraintConfExpr Name

-- |
-- ==== References
-- @
-- returning_clause:
--   | RETURNING target_list
--   | EMPTY
-- @
data ReturningClause rows where
  EmptyReturningClause :: ReturningClause '[]
  TargetListReturningClause :: TargetList (row ': rows) -> ReturningClause (row ': rows)

-- * Update

-- |
-- ==== References
-- @
-- UpdateStmt:
--   | opt_with_clause UPDATE relation_expr_opt_alias
--       SET set_clause_list
--       from_clause
--       where_or_current_clause
--       returning_clause
-- @
data UpdateStmt rows
  = UpdateStmt
      (Maybe WithClause)
      RelationExprOptAlias
      SetClauseList
      (Maybe FromClause)
      (Maybe WhereOrCurrentClause)
      (ReturningClause rows)

-- |
-- ==== References
-- @
-- set_clause_list:
--   | set_clause
--   | set_clause_list ',' set_clause
-- @
type SetClauseList = NonEmpty SetClause

-- |
-- ==== References
-- @
-- set_clause:
--   | set_target '=' a_expr
--   | '(' set_target_list ')' '=' a_expr
-- @
data SetClause
  = forall a. (IsAExpr a) => TargetSetClause SetTarget a
  | forall a. (IsAExpr a) => TargetListSetClause SetTargetList a

-- |
-- ==== References
-- @
-- set_target:
--   | ColId opt_indirection
-- @
data SetTarget = SetTarget ColId (Maybe Indirection)

-- |
-- ==== References
-- @
-- set_target_list:
--   | set_target
--   | set_target_list ',' set_target
-- @
type SetTargetList = NonEmpty SetTarget

-- * Delete

-- |
-- ==== References
-- @
-- DeleteStmt:
--   | opt_with_clause DELETE_P FROM relation_expr_opt_alias
--       using_clause where_or_current_clause returning_clause
-- @
data DeleteStmt rows
  = DeleteStmt
      (Maybe WithClause)
      RelationExprOptAlias
      (Maybe UsingClause)
      (Maybe WhereOrCurrentClause)
      (ReturningClause rows)

-- |
-- ==== References
-- @
-- using_clause:
--   | USING from_list
--   | EMPTY
-- @
type UsingClause = FromList

-- * Select

-- |
-- ==== References
-- @
-- SelectStmt:
--   |  select_no_parens
--   |  select_with_parens
-- @
type SelectStmt rows = Either (SelectNoParens rows) (SelectWithParens rows)

-- |
-- ==== References
-- @
-- select_with_parens:
--   |  '(' select_no_parens ')'
--   |  '(' select_with_parens ')'
-- @
data SelectWithParens rows
  = NoParensSelectWithParens (SelectNoParens rows)
  | WithParensSelectWithParens (SelectWithParens rows)

-- |
-- Covers the following cases:
--
-- @
-- select_no_parens:
--   |  simple_select
--   |  select_clause sort_clause
--   |  select_clause opt_sort_clause for_locking_clause opt_select_limit
--   |  select_clause opt_sort_clause select_limit opt_for_locking_clause
--   |  with_clause select_clause
--   |  with_clause select_clause sort_clause
--   |  with_clause select_clause opt_sort_clause for_locking_clause opt_select_limit
--   |  with_clause select_clause opt_sort_clause select_limit opt_for_locking_clause
-- @
data SelectNoParens rows
  = SelectNoParens (Maybe WithClause) (SelectClause rows) (Maybe SortClause) (Maybe SelectLimit) (Maybe ForLockingClause)

-- |
-- @
-- select_clause:
--   |  simple_select
--   |  select_with_parens
-- @
type SelectClause rows = Either (SimpleSelect rows) (SelectWithParens rows)

-- |
-- ==== References
-- @
-- simple_select:
--   |  SELECT opt_all_clause opt_target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--   |  SELECT distinct_clause target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--   |  values_clause
--   |  TABLE relation_expr
--   |  select_clause UNION all_or_distinct select_clause
--   |  select_clause INTERSECT all_or_distinct select_clause
--   |  select_clause EXCEPT all_or_distinct select_clause
-- @
data SimpleSelect rows
  = NormalSimpleSelect
      (Targeting rows)
      (Maybe IntoClause)
      (Maybe FromClause)
      (Maybe AExpr)
      (Maybe GroupClause)
      (Maybe AExpr)
      (Maybe WindowClause)
  | ValuesSimpleSelect (TargetTypesList rows) ValuesClause
  | TableSimpleSelect (TargetTypesList rows) RelationExpr
  | BinSimpleSelect SelectBinOp (SelectClause rows) (Maybe Bool) (SelectClause rows)

-- |
-- Covers these parts of spec:
--
-- ==== References
-- @
-- simple_select:
--   |  SELECT opt_all_clause opt_target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--   |  SELECT distinct_clause target_list
--       into_clause from_clause where_clause
--       group_clause having_clause window_clause
--
-- distinct_clause:
--   |  DISTINCT
--   |  DISTINCT ON '(' expr_list ')'
-- @
data Targeting rows where
  EmptyTargeting :: Targeting '[]
  NormalTargeting :: TargetList (row ': rows) -> Targeting (row ': rows)
  AllTargeting :: TargetList (row ': rows) -> Targeting (row ': rows)
  DistinctTargeting :: Maybe ExprList -> TargetList (row ': rows) -> Targeting (row ': rows)

-- |
-- ==== References
-- @
-- target_list:
--   | target_el
--   | target_list ',' target_el
-- @
data TargetList rows where
  TargetListNil :: TargetList '[]
  (:.) :: (IsTargetElRow a) => a -> TargetList rows -> TargetList (TargetRow a ': rows)

infixr 5 :.

-- |
-- ==== References
-- @
-- target_el:
--   |  a_expr AS ColLabel
--   |  a_expr IDENT
--   |  a_expr
--   |  '*'
-- @
data TargetElRow row where
  TargetElRow :: (IsTargetEl el) => TargetTypes (a ': row) -> el -> TargetElRow (a ': row)

class IsTargetElRow a where
  type TargetRow a :: [(Type, Type)]
  toTargetElRow :: a -> TargetElRow (TargetRow a)

instance IsTargetElRow (TargetElRow row) where
  type TargetRow (TargetElRow row) = row
  toTargetElRow = id

-- |
-- ==== References
-- @
-- target_el:
--   |  a_expr AS ColLabel
--   |  a_expr IDENT
--   |  a_expr
--   |  '*'
-- @
data TargetEl
  = forall a. (IsAExpr a) => AliasedExprTargetEl a Ident
  | forall a. (IsAExpr a) => ImplicitlyAliasedExprTargetEl a Ident
  | ExprTargetEl AExpr
  | AsteriskTargetEl

class IsTargetEl a where
  toTargetEl :: a -> TargetEl
  default toTargetEl :: (IsAExpr a) => a -> TargetEl
  toTargetEl = ExprTargetEl . toAExpr

instance IsTargetEl TargetEl where toTargetEl = id
instance IsTargetEl AExpr
instance IsTargetEl CExpr
instance IsTargetEl Columnref
instance IsTargetEl AexprConst
instance IsTargetEl Param
instance IsTargetEl CaseExpr
instance IsTargetEl FuncExpr
instance IsTargetEl (SelectWithParens rows)
instance IsTargetEl ArrayExpr
instance IsTargetEl ExplicitRow
instance IsTargetEl ImplicitRow
instance IsTargetEl Iconst
instance IsTargetEl Fconst
instance IsTargetEl Bool

-- |
-- ==== References
-- @
-- with_clause:
--   |  WITH cte_list
--   |  WITH_LA cte_list
--   |  WITH RECURSIVE cte_list
-- @
data WithClause = WithClause Bool (NonEmpty CommonTableExpr)

-- |
-- ==== References
-- @
-- common_table_expr:
--   |  name opt_name_list AS opt_materialized '(' PreparableStmt ')'
-- opt_materialized:
--   | MATERIALIZED
--   | NOT MATERIALIZED
--   | EMPTY
-- @
data CommonTableExpr = forall rows. CommonTableExpr Ident (Maybe (NonEmpty Ident)) (Maybe Bool) (PreparableStmt rows)

type IntoClause = OptTempTableName

-- |
-- ==== References
-- @
-- OptTempTableName:
--   |  TEMPORARY opt_table qualified_name
--   |  TEMP opt_table qualified_name
--   |  LOCAL TEMPORARY opt_table qualified_name
--   |  LOCAL TEMP opt_table qualified_name
--   |  GLOBAL TEMPORARY opt_table qualified_name
--   |  GLOBAL TEMP opt_table qualified_name
--   |  UNLOGGED opt_table qualified_name
--   |  TABLE qualified_name
--   |  qualified_name
-- @
data OptTempTableName
  = TemporaryOptTempTableName Bool QualifiedName
  | TempOptTempTableName Bool QualifiedName
  | LocalTemporaryOptTempTableName Bool QualifiedName
  | LocalTempOptTempTableName Bool QualifiedName
  | GlobalTemporaryOptTempTableName Bool QualifiedName
  | GlobalTempOptTempTableName Bool QualifiedName
  | UnloggedOptTempTableName Bool QualifiedName
  | TableOptTempTableName QualifiedName
  | QualifiedOptTempTableName QualifiedName

type FromClause = NonEmpty TableRef

type GroupClause = NonEmpty GroupByItem

-- |
-- ==== References
-- @
-- group_by_item:
--   |  a_expr
--   |  empty_grouping_set
--   |  cube_clause
--   |  rollup_clause
--   |  grouping_sets_clause
-- empty_grouping_set:
--   |  '(' ')'
-- rollup_clause:
--   |  ROLLUP '(' expr_list ')'
-- cube_clause:
--   |  CUBE '(' expr_list ')'
-- grouping_sets_clause:
--   |  GROUPING SETS '(' group_by_list ')'
-- @
data GroupByItem
  = ExprGroupByItem AExpr
  | EmptyGroupingSetGroupByItem
  | RollupGroupByItem ExprList
  | CubeGroupByItem ExprList
  | GroupingSetsGroupByItem (NonEmpty GroupByItem)

class IsGroupByItem a where toGroupByItem :: a -> GroupByItem
instance IsGroupByItem GroupByItem where toGroupByItem = id
instance IsGroupByItem AExpr where toGroupByItem = ExprGroupByItem
instance IsGroupByItem CExpr where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem Columnref where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem AexprConst where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem Param where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem CaseExpr where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem FuncExpr where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem (SelectWithParens rows) where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem ArrayExpr where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem ExplicitRow where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem ImplicitRow where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem Iconst where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem Fconst where toGroupByItem = toGroupByItem . toAExpr
instance IsGroupByItem Bool where toGroupByItem = toGroupByItem . toAExpr

-- |
-- @
-- window_clause:
--   |  WINDOW window_definition_list
--   |  EMPTY
--
-- window_definition_list:
--   |  window_definition
--   |  window_definition_list ',' window_definition
-- @
type WindowClause = NonEmpty WindowDefinition

-- |
-- @
-- window_definition:
--   |  ColId AS window_specification
-- @
data WindowDefinition = WindowDefinition Ident WindowSpecification

-- |
-- @
-- window_specification:
--   |  '(' opt_existing_window_name opt_partition_clause
--             opt_sort_clause opt_frame_clause ')'
--
-- opt_existing_window_name:
--   |  ColId
--   |  EMPTY
--
-- opt_partition_clause:
--   |  PARTITION BY expr_list
--   |  EMPTY
-- @
data WindowSpecification
  = WindowSpecification (Maybe ExistingWindowName) (Maybe PartitionClause) (Maybe SortClause) (Maybe FrameClause)

type PartitionClause = ExprList

-- |
-- ==== References
-- @
-- opt_frame_clause:
--   |  RANGE frame_extent opt_window_exclusion_clause
--   |  ROWS frame_extent opt_window_exclusion_clause
--   |  GROUPS frame_extent opt_window_exclusion_clause
--   |  EMPTY
-- @
data FrameClause = FrameClause FrameClauseMode FrameExtent (Maybe WindowExclusionClause)

-- |
-- ==== References
-- @
-- frame_extent:
--   |  frame_bound
--   |  BETWEEN frame_bound AND frame_bound
-- @
data FrameExtent = SingularFrameExtent FrameBound | BetweenFrameExtent FrameBound FrameBound

-- |
-- ==== References
-- @
-- frame_bound:
--   |  UNBOUNDED PRECEDING
--   |  UNBOUNDED FOLLOWING
--   |  CURRENT_P rows
--   |  a_expr PRECEDING
--   |  a_expr FOLLOWING
-- @
data FrameBound
  = UnboundedPrecedingFrameBound
  | UnboundedFollowingFrameBound
  | CurrentRowFrameBound
  | forall a. (IsAExpr a) => PrecedingFrameBound a
  | forall a. (IsAExpr a) => FollowingFrameBound a

-- |
-- ==== References
-- @
-- values_clause:
--   |  VALUES '(' expr_list ')'
--   |  values_clause ',' '(' expr_list ')'
-- @
type ValuesClause = NonEmpty ExprList

-- |
--
-- sort_clause:
--   |  ORDER BY sortby_list
--
-- sortby_list:
--   |  sortby
--   |  sortby_list ',' sortby
type SortClause = NonEmpty SortBy

-- |
-- ==== References
-- @
-- sortby:
--   |  a_expr USING qual_all_Op opt_nulls_order
--   |  a_expr opt_asc_desc opt_nulls_order
-- @
data SortBy
  = forall a. (IsAExpr a) => UsingSortBy a QualAllOp (Maybe NullsOrder)
  | forall a. (IsAExpr a) => AscDescSortBy a (Maybe AscDesc) (Maybe NullsOrder)

-- |
-- ==== References
-- @
-- select_limit:
--   | limit_clause offset_clause
--   | offset_clause limit_clause
--   | limit_clause
--   | offset_clause
-- @
data SelectLimit
  = forall a b. (IsLimitClause a, IsOffsetClause b) => LimitOffsetSelectLimit a b
  | forall a b. (IsOffsetClause a, IsLimitClause b) => OffsetLimitSelectLimit a b
  | forall a. (IsLimitClause a) => LimitSelectLimit a
  | forall a. (IsOffsetClause a) => OffsetSelectLimit a

-- |
-- ==== References
-- @
-- limit_clause:
--   | LIMIT select_limit_value
--   | LIMIT select_limit_value ',' select_offset_value
--   | FETCH first_or_next select_fetch_first_value row_or_rows ONLY
--   | FETCH first_or_next row_or_rows ONLY
-- select_offset_value:
--   | a_expr
-- first_or_next:
--   | FIRST_P
--   | NEXT
-- row_or_rows:
--   | rows
--   | ROWS
-- @
data LimitClause
  = forall a. (IsSelectLimitValue a) => LimitLimitClause a (Maybe AExpr)
  | FetchOnlyLimitClause Bool (Maybe SelectFetchFirstValue) Bool

class IsLimitClause a where
  toLimitClause :: a -> LimitClause
  default toLimitClause :: (IsSelectLimitValue a) => a -> LimitClause
  toLimitClause = flip LimitLimitClause Nothing . toSelectLimitValue

instance IsLimitClause LimitClause where toLimitClause = id
instance IsLimitClause SelectLimitValue
instance IsLimitClause AExpr
instance IsLimitClause CExpr
instance IsLimitClause Columnref
instance IsLimitClause AexprConst
instance IsLimitClause Param
instance IsLimitClause CaseExpr
instance IsLimitClause FuncExpr
instance IsLimitClause (SelectWithParens rows)
instance IsLimitClause ArrayExpr
instance IsLimitClause ExplicitRow
instance IsLimitClause ImplicitRow
instance IsLimitClause Iconst
instance IsLimitClause Fconst
instance IsLimitClause Bool

-- |
-- ==== References
-- @
-- select_fetch_first_value:
--   | c_expr
--   | '+' I_or_F_const
--   | '-' I_or_F_const
-- @
data SelectFetchFirstValue
  = ExprSelectFetchFirstValue CExpr
  | NumSelectFetchFirstValue Bool (Either Int64 Double)

class IsSelectFetchFirstValue a where toSelectFetchFirstValue :: a -> SelectFetchFirstValue
instance IsSelectFetchFirstValue SelectFetchFirstValue where toSelectFetchFirstValue = id
instance IsSelectFetchFirstValue CExpr where toSelectFetchFirstValue = ExprSelectFetchFirstValue
instance IsSelectFetchFirstValue Columnref where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue AexprConst where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue Param where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue CaseExpr where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue FuncExpr where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue (SelectWithParens rows) where
  toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue ArrayExpr where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue ExplicitRow where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue ImplicitRow where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue Iconst where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue Fconst where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr
instance IsSelectFetchFirstValue Bool where toSelectFetchFirstValue = toSelectFetchFirstValue . toCExpr

-- |
-- ==== References
-- @
-- select_limit_value:
--   | a_expr
--   | ALL
-- @
data SelectLimitValue
  = ExprSelectLimitValue AExpr
  | AllSelectLimitValue

class IsSelectLimitValue a where toSelectLimitValue :: a -> SelectLimitValue
instance IsSelectLimitValue SelectLimitValue where toSelectLimitValue = id
instance IsSelectLimitValue AExpr where toSelectLimitValue = ExprSelectLimitValue
instance IsSelectLimitValue CExpr where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue Columnref where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue AexprConst where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue Param where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue CaseExpr where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue FuncExpr where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue (SelectWithParens rows) where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue ArrayExpr where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue ExplicitRow where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue ImplicitRow where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue Iconst where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue Fconst where toSelectLimitValue = toSelectLimitValue . toAExpr
instance IsSelectLimitValue Bool where toSelectLimitValue = toSelectLimitValue . toAExpr

-- |
-- ==== References
-- @
-- offset_clause:
--   | OFFSET select_offset_value
--   | OFFSET select_fetch_first_value row_or_rows
-- select_offset_value:
--   | a_expr
-- row_or_rows:
--   | rows
--   | ROWS
-- @
data OffsetClause
  = ExprOffsetClause AExpr
  | forall a. (IsSelectFetchFirstValue a) => FetchFirstOffsetClause a Bool

class IsOffsetClause a where toOffsetClause :: a -> OffsetClause
instance IsOffsetClause OffsetClause where toOffsetClause = id
instance IsOffsetClause AExpr where toOffsetClause = ExprOffsetClause
instance IsOffsetClause CExpr where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause Columnref where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause AexprConst where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause Param where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause CaseExpr where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause FuncExpr where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause (SelectWithParens rows) where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause ArrayExpr where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause ExplicitRow where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause ImplicitRow where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause Iconst where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause Fconst where toOffsetClause = toOffsetClause . toAExpr
instance IsOffsetClause Bool where toOffsetClause = toOffsetClause . toAExpr

-- * For Locking

-- |
-- ==== References
-- @
-- for_locking_clause:
--   | for_locking_items
--   | FOR READ ONLY
-- for_locking_items:
--   | for_locking_item
--   | for_locking_items for_locking_item
-- @
data ForLockingClause
  = ItemsForLockingClause (NonEmpty ForLockingItem)
  | ReadOnlyForLockingClause

-- |
-- ==== References
-- @
-- for_locking_item:
--   | for_locking_strength locked_rels_list opt_nowait_or_skip
-- locked_rels_list:
--   | OF qualified_name_list
--   | EMPTY
-- opt_nowait_or_skip:
--   | NOWAIT
--   | SKIP LOCKED
--   | EMPTY
-- @
data ForLockingItem = ForLockingItem ForLockingStrength (Maybe (NonEmpty QualifiedName)) (Maybe Bool)

-- * Table references and joining

-- |
-- ==== References
-- @
-- from_list:
--   | table_ref
--   | from_list ',' table_ref
-- @
type FromList = NonEmpty TableRef

-- |
-- ==== References
-- @
-- | relation_expr opt_alias_clause
-- | relation_expr opt_alias_clause tablesample_clause
-- | func_table func_alias_clause
-- | LATERAL_P func_table func_alias_clause
-- | xmltable opt_alias_clause
-- | LATERAL_P xmltable opt_alias_clause
-- | select_with_parens opt_alias_clause
-- | LATERAL_P select_with_parens opt_alias_clause
-- | joined_table
-- | '(' joined_table ')' alias_clause
--
-- TODO: Add xmltable
-- @
data TableRef
  = -- |
    -- @
    --    | relation_expr opt_alias_clause
    --    | relation_expr opt_alias_clause tablesample_clause
    -- @
    RelationExprTableRef RelationExpr (Maybe AliasClause) (Maybe TablesampleClause)
  | -- |
    -- @
    --    | func_table func_alias_clause
    --    | LATERAL_P func_table func_alias_clause
    -- @
    FuncTableRef Bool FuncTable (Maybe FuncAliasClause)
  | -- |
    -- @
    --    | select_with_parens opt_alias_clause
    --    | LATERAL_P select_with_parens opt_alias_clause
    -- @
    forall rows. SelectTableRef Bool (SelectWithParens rows) (Maybe AliasClause)
  | -- |
    -- @
    --    | joined_table
    --    | '(' joined_table ')' alias_clause
    -- @
    JoinTableRef JoinedTable (Maybe AliasClause)

class IsTableRef a where toTableRef :: a -> TableRef
instance IsTableRef TableRef where toTableRef = id
instance IsTableRef FuncTable where toTableRef = flip (FuncTableRef False) Nothing
instance IsTableRef RelationExpr where toTableRef a = RelationExprTableRef a Nothing Nothing
instance IsTableRef JoinedTable where toTableRef a = JoinTableRef a Nothing

joinTable :: (IsTableRef l, IsTableRef r) => JoinMeth -> l -> r -> TableRef
joinTable meth l r = JoinTableRef (MethJoinedTable meth l r) Nothing

-- |
-- ==== References
-- @
-- | qualified_name
-- | qualified_name '*'
-- | ONLY qualified_name
-- | ONLY '(' qualified_name ')'
-- @
data RelationExpr
  = SimpleRelationExpr
      QualifiedName
      -- ^ Name.
      Bool
      -- ^ Is asterisk present?
  | OnlyRelationExpr
      QualifiedName
      -- ^ Name.
      Bool
      -- ^ Are parentheses present?

-- |
-- ==== References
-- @
-- relation_expr_opt_alias:
--   | relation_expr
--   | relation_expr ColId
--   | relation_expr AS ColId
-- @
data RelationExprOptAlias = RelationExprOptAlias RelationExpr (Maybe (Bool, ColId))

-- |
-- ==== References
-- @
-- tablesample_clause:
--   | TABLESAMPLE func_name '(' expr_list ')' opt_repeatable_clause
-- @
data TablesampleClause = TablesampleClause FuncName ExprList (Maybe AExpr)

-- |
-- ==== References
-- @
-- func_table:
--   | func_expr_windowless opt_ordinality
--   | ROWS FROM '(' rowsfrom_list ')' opt_ordinality
-- @
data FuncTable
  = FuncExprFuncTable FuncExprWindowless OptOrdinality
  | RowsFromFuncTable RowsFromList OptOrdinality

-- |
-- ==== References
-- @
-- rowsfrom_item:
--   | func_expr_windowless opt_col_def_list
-- @
data RowsFromItem = RowsFromItem FuncExprWindowless (Maybe ColDefList)

-- |
-- ==== References
-- @
-- rowsfrom_list:
--   | rowsfrom_item
--   | rowsfrom_list ',' rowsfrom_item
-- @
type RowsFromList = NonEmpty RowsFromItem

-- |
-- ==== References
-- @
-- opt_col_def_list:
--   | AS '(' TableFuncElementList ')'
--   | EMPTY
-- @
type ColDefList = TableFuncElementList

-- |
-- ==== References
-- @
-- TableFuncElementList:
--   | TableFuncElement
--   | TableFuncElementList ',' TableFuncElement
-- @
type TableFuncElementList = NonEmpty TableFuncElement

-- |
-- ==== References
-- @
-- TableFuncElement:
--   | ColId Typename opt_collate_clause
-- @
data TableFuncElement = TableFuncElement ColId Typename (Maybe CollateClause)

-- |
-- ==== References
-- @
-- func_alias_clause:
--   | alias_clause
--   | AS '(' TableFuncElementList ')'
--   | AS ColId '(' TableFuncElementList ')'
--   | ColId '(' TableFuncElementList ')'
--   | EMPTY
-- @
data FuncAliasClause
  = AliasFuncAliasClause AliasClause
  | AsFuncAliasClause TableFuncElementList
  | AsColIdFuncAliasClause ColId TableFuncElementList
  | ColIdFuncAliasClause ColId TableFuncElementList

-- |
-- ==== References
-- @
-- | '(' joined_table ')'
-- | table_ref CROSS JOIN table_ref
-- | table_ref join_type JOIN table_ref join_qual
-- | table_ref JOIN table_ref join_qual
-- | table_ref NATURAL join_type JOIN table_ref
-- | table_ref NATURAL JOIN table_ref
--
-- The options are covered by the `JoinMeth` type.
-- @
data JoinedTable
  = InParensJoinedTable JoinedTable
  | forall a b. (IsTableRef a, IsTableRef b) => MethJoinedTable JoinMeth a b

-- |
-- ==== References
-- @
-- | table_ref CROSS JOIN table_ref
-- | table_ref join_type JOIN table_ref join_qual
-- | table_ref JOIN table_ref join_qual
-- | table_ref NATURAL join_type JOIN table_ref
-- | table_ref NATURAL JOIN table_ref
-- @
data JoinMeth
  = CrossJoinMeth
  | QualJoinMeth (Maybe JoinType) JoinQual
  | NaturalJoinMeth (Maybe JoinType)

-- |
-- ==== References
-- @
-- join_qual:
--   |  USING '(' name_list ')'
--   |  ON a_expr
-- @
data JoinQual
  = UsingJoinQual (NonEmpty Ident)
  | forall a. (IsAExpr a) => OnJoinQual a

-- * Where

-- |
-- ==== References
-- @
-- | WHERE a_expr
-- | WHERE CURRENT_P OF cursor_name
-- | /*EMPTY*/
-- @
data WhereOrCurrentClause
  = ExprWhereOrCurrentClause AExpr
  | CursorWhereOrCurrentClause CursorName

class IsWhereOrCurrentClause a where toWhereOrCurrentClause :: a -> WhereOrCurrentClause
instance IsWhereOrCurrentClause WhereOrCurrentClause where toWhereOrCurrentClause = id
instance IsWhereOrCurrentClause AExpr where toWhereOrCurrentClause = ExprWhereOrCurrentClause
instance IsWhereOrCurrentClause CursorName where toWhereOrCurrentClause = CursorWhereOrCurrentClause
instance IsWhereOrCurrentClause CExpr where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause Columnref where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause AexprConst where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause Param where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause CaseExpr where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause FuncExpr where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause (SelectWithParens rows) where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause ArrayExpr where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause ExplicitRow where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause ImplicitRow where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause Iconst where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause Fconst where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr
instance IsWhereOrCurrentClause Bool where toWhereOrCurrentClause = toWhereOrCurrentClause . toAExpr

-- * Expression

type ExprList = NonEmpty AExpr

-- |
-- ==== References
-- @
-- a_expr:
--   | c_expr
--   | a_expr TYPECAST Typename
--   | a_expr COLLATE any_name
--   | a_expr AT TIME ZONE a_expr
--   | '+' a_expr
--   | '-' a_expr
--   | a_expr '+' a_expr
--   | a_expr '-' a_expr
--   | a_expr '*' a_expr
--   | a_expr '/' a_expr
--   | a_expr '%' a_expr
--   | a_expr '^' a_expr
--   | a_expr '<' a_expr
--   | a_expr '>' a_expr
--   | a_expr '=' a_expr
--   | a_expr LESS_EQUALS a_expr
--   | a_expr GREATER_EQUALS a_expr
--   | a_expr NOT_EQUALS a_expr
--   | a_expr qual_Op a_expr
--   | qual_Op a_expr
--   | a_expr qual_Op
--   | a_expr AND a_expr
--   | a_expr OR a_expr
--   | NOT a_expr
--   | NOT_LA a_expr
--   | a_expr LIKE a_expr
--   | a_expr LIKE a_expr ESCAPE a_expr
--   | a_expr NOT_LA LIKE a_expr
--   | a_expr NOT_LA LIKE a_expr ESCAPE a_expr
--   | a_expr ILIKE a_expr
--   | a_expr ILIKE a_expr ESCAPE a_expr
--   | a_expr NOT_LA ILIKE a_expr
--   | a_expr NOT_LA ILIKE a_expr ESCAPE a_expr
--   | a_expr SIMILAR TO a_expr
--   | a_expr SIMILAR TO a_expr ESCAPE a_expr
--   | a_expr NOT_LA SIMILAR TO a_expr
--   | a_expr NOT_LA SIMILAR TO a_expr ESCAPE a_expr
--   | a_expr IS NULL_P
--   | a_expr ISNULL
--   | a_expr IS NOT NULL_P
--   | a_expr NOTNULL
--   | rows OVERLAPS rows
--   | a_expr IS TRUE_P
--   | a_expr IS NOT TRUE_P
--   | a_expr IS FALSE_P
--   | a_expr IS NOT FALSE_P
--   | a_expr IS UNKNOWN
--   | a_expr IS NOT UNKNOWN
--   | a_expr IS DISTINCT FROM a_expr
--   | a_expr IS NOT DISTINCT FROM a_expr
--   | a_expr IS OF '(' type_list ')'
--   | a_expr IS NOT OF '(' type_list ')'
--   | a_expr BETWEEN opt_asymmetric b_expr AND a_expr
--   | a_expr NOT_LA BETWEEN opt_asymmetric b_expr AND a_expr
--   | a_expr BETWEEN SYMMETRIC b_expr AND a_expr
--   | a_expr NOT_LA BETWEEN SYMMETRIC b_expr AND a_expr
--   | a_expr IN_P in_expr
--   | a_expr NOT_LA IN_P in_expr
--   | a_expr subquery_Op sub_type select_with_parens
--   | a_expr subquery_Op sub_type '(' a_expr ')'
--   | UNIQUE select_with_parens
--   | a_expr IS DOCUMENT_P
--   | a_expr IS NOT DOCUMENT_P
--   | DEFAULT
-- @
data AExpr
  = CExprAExpr CExpr
  | forall a. (IsAExpr a) => TypecastAExpr a Typename
  | forall a. (IsAExpr a) => CollateAExpr a AnyName
  | forall a b. (IsAExpr a, IsAExpr b) => AtTimeZoneAExpr a b
  | forall a. (IsAExpr a) => PlusAExpr a
  | forall a. (IsAExpr a) => MinusAExpr a
  | forall a b. (IsAExpr a, IsAExpr b) => SymbolicBinOpAExpr a SymbolicExprBinOp b
  | forall a. (IsAExpr a) => PrefixQualOpAExpr QualOp a
  | forall a. (IsAExpr a) => SuffixQualOpAExpr a QualOp
  | forall a b. (IsAExpr a, IsAExpr b) => AndAExpr a b
  | forall a b. (IsAExpr a, IsAExpr b) => OrAExpr a b
  | forall a. (IsAExpr a) => NotAExpr a
  | forall a b. (IsAExpr a, IsAExpr b) => VerbalExprBinOpAExpr a Bool VerbalExprBinOp b (Maybe AExpr)
  | forall a. (IsAExpr a) => ReversableOpAExpr a Bool AExprReversableOp
  | forall a. (IsAExpr a) => IsnullAExpr a
  | forall a. (IsAExpr a) => NotnullAExpr a
  | OverlapsAExpr Row Row
  | forall a b rows. (IsAExpr a, IsAExpr b) => SubqueryAExpr a SubqueryOp SubType (Either (SelectWithParens rows) b)
  | forall rows. UniqueAExpr (SelectWithParens rows)
  | DefaultAExpr

class IsAExpr a where toAExpr :: a -> AExpr
instance IsAExpr AExpr where toAExpr = id
instance IsAExpr CExpr where toAExpr = CExprAExpr
instance IsAExpr Columnref where toAExpr = toAExpr . toCExpr
instance IsAExpr AexprConst where toAExpr = toAExpr . toCExpr
instance IsAExpr Param where toAExpr = toAExpr . toCExpr
instance IsAExpr CaseExpr where toAExpr = toAExpr . toCExpr
instance IsAExpr FuncExpr where toAExpr = toAExpr . toCExpr
instance IsAExpr (SelectWithParens rows) where toAExpr = toAExpr . toCExpr
instance IsAExpr ArrayExpr where toAExpr = toAExpr . toCExpr
instance IsAExpr ExplicitRow where toAExpr = toAExpr . toCExpr
instance IsAExpr ImplicitRow where toAExpr = toAExpr . toCExpr
instance IsAExpr Iconst where toAExpr = toAExpr . toCExpr
instance IsAExpr Fconst where toAExpr = toAExpr . toCExpr
instance IsAExpr Bool where toAExpr = toAExpr . toCExpr

-- |
-- ==== References
-- @
-- b_expr:
--   | c_expr
--   | b_expr TYPECAST Typename
--   | '+' b_expr
--   | '-' b_expr
--   | b_expr '+' b_expr
--   | b_expr '-' b_expr
--   | b_expr '*' b_expr
--   | b_expr '/' b_expr
--   | b_expr '%' b_expr
--   | b_expr '^' b_expr
--   | b_expr '<' b_expr
--   | b_expr '>' b_expr
--   | b_expr '=' b_expr
--   | b_expr LESS_EQUALS b_expr
--   | b_expr GREATER_EQUALS b_expr
--   | b_expr NOT_EQUALS b_expr
--   | b_expr qual_Op b_expr
--   | qual_Op b_expr
--   | b_expr qual_Op
--   | b_expr IS DISTINCT FROM b_expr
--   | b_expr IS NOT DISTINCT FROM b_expr
--   | b_expr IS OF '(' type_list ')'
--   | b_expr IS NOT OF '(' type_list ')'
--   | b_expr IS DOCUMENT_P
--   | b_expr IS NOT DOCUMENT_P
-- @
data BExpr
  = CExprBExpr CExpr
  | forall a. (IsBExpr a) => TypecastBExpr a Typename
  | forall a. (IsBExpr a) => PlusBExpr a
  | forall a. (IsBExpr a) => MinusBExpr a
  | forall a b. (IsBExpr a, IsBExpr b) => SymbolicBinOpBExpr a SymbolicExprBinOp b
  | forall a. (IsBExpr a) => QualOpBExpr QualOp a
  | forall a. (IsBExpr a) => IsOpBExpr a Bool BExprIsOp

class IsBExpr a where toBExpr :: a -> BExpr
instance IsBExpr BExpr where toBExpr = id
instance IsBExpr CExpr where toBExpr = CExprBExpr
instance IsBExpr Columnref where toBExpr = toBExpr . toCExpr
instance IsBExpr AexprConst where toBExpr = toBExpr . toCExpr
instance IsBExpr Param where toBExpr = toBExpr . toCExpr
instance IsBExpr CaseExpr where toBExpr = toBExpr . toCExpr
instance IsBExpr FuncExpr where toBExpr = toBExpr . toCExpr
instance IsBExpr (SelectWithParens rows) where toBExpr = toBExpr . toCExpr
instance IsBExpr ArrayExpr where toBExpr = toBExpr . toCExpr
instance IsBExpr ExplicitRow where toBExpr = toBExpr . toCExpr
instance IsBExpr ImplicitRow where toBExpr = toBExpr . toCExpr
instance IsBExpr Iconst where toBExpr = toBExpr . toCExpr
instance IsBExpr Fconst where toBExpr = toBExpr . toCExpr
instance IsBExpr Bool where toBExpr = toBExpr . toCExpr

newtype Param = Param Internal.Param

-- |
-- ==== References
-- @
-- c_expr:
--   | columnref
--   | AexprConst
--   | PARAM opt_indirection
--   | '(' a_expr ')' opt_indirection
--   | case_expr
--   | func_expr
--   | select_with_parens
--   | select_with_parens indirection
--   | EXISTS select_with_parens
--   | ARRAY select_with_parens
--   | ARRAY array_expr
--   | explicit_row
--   | implicit_row
--   | GROUPING '(' expr_list ')'
-- @
data CExpr
  = ColumnrefCExpr Columnref
  | AexprConstCExpr AexprConst
  | ParamCExpr Param
  | forall a. (IsAExpr a) => InParensCExpr a (Maybe Indirection)
  | CaseCExpr CaseExpr
  | FuncCExpr FuncExpr
  | forall rows. SelectWithParensCExpr (SelectWithParens rows) (Maybe Indirection)
  | forall rows. ExistsCExpr (SelectWithParens rows)
  | forall rows. ArrayCExpr (Either (SelectWithParens rows) ArrayExpr)
  | ExplicitRowCExpr ExplicitRow
  | ImplicitRowCExpr ImplicitRow
  | GroupingCExpr ExprList

class IsCExpr a where toCExpr :: a -> CExpr
instance IsCExpr CExpr where toCExpr = id
instance IsCExpr Columnref where toCExpr = ColumnrefCExpr
instance IsCExpr AexprConst where toCExpr = AexprConstCExpr
instance IsCExpr Param where toCExpr = ParamCExpr
instance IsCExpr CaseExpr where toCExpr = CaseCExpr
instance IsCExpr FuncExpr where toCExpr = FuncCExpr
instance IsCExpr (SelectWithParens rows) where toCExpr = flip SelectWithParensCExpr Nothing
instance IsCExpr ArrayExpr where toCExpr = ArrayCExpr . Right
instance IsCExpr ExplicitRow where toCExpr = ExplicitRowCExpr
instance IsCExpr ImplicitRow where toCExpr = ImplicitRowCExpr
instance IsCExpr Iconst where toCExpr = toCExpr . toAexprConst
instance IsCExpr Fconst where toCExpr = toCExpr . toAexprConst
instance IsCExpr Bool where toCExpr = toCExpr . toAexprConst

-- |
-- ==== References
-- @
-- in_expr:
--   | select_with_parens
--   | '(' expr_list ')'
-- @
data InExpr
  = forall rows. SelectInExpr (SelectWithParens rows)
  | ExprListInExpr ExprList

-- |
-- ==== References
-- @
-- array_expr:
--   | '[' expr_list ']'
--   | '[' array_expr_list ']'
--   | '[' ']'
-- @
data ArrayExpr
  = ExprListArrayExpr ExprList
  | ArrayExprListArrayExpr ArrayExprList
  | EmptyArrayExpr

-- |
-- ==== References
-- @
-- array_expr_list:
--   | array_expr
--   | array_expr_list ',' array_expr
-- @
type ArrayExprList = NonEmpty ArrayExpr

-- |
-- ==== References
-- @
-- rows:
--   | rows '(' expr_list ')'
--   | rows '(' ')'
--   | '(' expr_list ',' a_expr ')'
-- @
data Row
  = ExplicitRowRow ExplicitRow
  | ImplicitRowRow ImplicitRow

-- |
-- ==== References
-- @
-- explicit_row:
--   | rows '(' expr_list ')'
--   | rows '(' ')'
-- @
type ExplicitRow = Maybe ExprList

-- |
-- ==== References
-- @
-- implicit_row:
--   | '(' expr_list ',' a_expr ')'
-- @
data ImplicitRow = forall a. (IsAExpr a) => ImplicitRow ExprList a

-- |
-- ==== References
-- @
-- func_expr:
--   | func_application within_group_clause filter_clause over_clause
--   | func_expr_common_subexpr
-- @
data FuncExpr
  = ApplicationFuncExpr FuncApplication (Maybe WithinGroupClause) (Maybe AExpr) (Maybe OverClause)
  | SubexprFuncExpr FuncExprCommonSubexpr

-- |
-- ==== References
-- @
-- func_expr_windowless:
--   | func_application
--   | func_expr_common_subexpr
-- @
data FuncExprWindowless
  = ApplicationFuncExprWindowless FuncApplication
  | CommonSubexprFuncExprWindowless FuncExprCommonSubexpr

-- |
-- ==== References
-- @
-- within_group_clause:
--   | WITHIN GROUP_P '(' sort_clause ')'
--   | EMPTY
-- @
type WithinGroupClause = SortClause

-- |
-- ==== References
-- @
-- over_clause:
--   | OVER window_specification
--   | OVER ColId
--   | EMPTY
-- @
data OverClause
  = WindowOverClause WindowSpecification
  | ColIdOverClause ColId

-- |
-- ==== References
-- @
-- func_expr_common_subexpr:
--   | COLLATION FOR '(' a_expr ')'
--   | CURRENT_DATE
--   | CURRENT_TIME
--   | CURRENT_TIME '(' Iconst ')'
--   | CURRENT_TIMESTAMP
--   | CURRENT_TIMESTAMP '(' Iconst ')'
--   | LOCALTIME
--   | LOCALTIME '(' Iconst ')'
--   | LOCALTIMESTAMP
--   | LOCALTIMESTAMP '(' Iconst ')'
--   | CURRENT_ROLE
--   | CURRENT_USER
--   | SESSION_USER
--   | USER
--   | CURRENT_CATALOG
--   | CURRENT_SCHEMA
--   | CAST '(' a_expr AS Typename ')'
--   | EXTRACT '(' extract_list ')'
--   | OVERLAY '(' overlay_list ')'
--   | POSITION '(' position_list ')'
--   | SUBSTRING '(' substr_list ')'
--   | TREAT '(' a_expr AS Typename ')'
--   | TRIM '(' BOTH trim_list ')'
--   | TRIM '(' LEADING trim_list ')'
--   | TRIM '(' TRAILING trim_list ')'
--   | TRIM '(' trim_list ')'
--   | NULLIF '(' a_expr ',' a_expr ')'
--   | COALESCE '(' expr_list ')'
--   | GREATEST '(' expr_list ')'
--   | LEAST '(' expr_list ')'
--   | XMLCONCAT '(' expr_list ')'
--   | XMLELEMENT '(' NAME_P ColLabel ')'
--   | XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ')'
--   | XMLELEMENT '(' NAME_P ColLabel ',' expr_list ')'
--   | XMLELEMENT '(' NAME_P ColLabel ',' xml_attributes ',' expr_list ')'
--   | XMLEXISTS '(' c_expr xmlexists_argument ')'
--   | XMLFOREST '(' xml_attribute_list ')'
--   | XMLPARSE '(' document_or_content a_expr xml_whitespace_option ')'
--   | XMLPI '(' NAME_P ColLabel ')'
--   | XMLPI '(' NAME_P ColLabel ',' a_expr ')'
--   | XMLROOT '(' a_expr ',' xml_root_version opt_xml_root_standalone ')'
--   | XMLSERIALIZE '(' document_or_content a_expr AS SimpleTypename ')'
--
-- TODO: Implement the XML cases
-- @
data FuncExprCommonSubexpr
  = forall a. (IsAExpr a) => CollationForFuncExprCommonSubexpr a
  | CurrentDateFuncExprCommonSubexpr
  | CurrentTimeFuncExprCommonSubexpr (Maybe Int64)
  | CurrentTimestampFuncExprCommonSubexpr (Maybe Int64)
  | LocalTimeFuncExprCommonSubexpr (Maybe Int64)
  | LocalTimestampFuncExprCommonSubexpr (Maybe Int64)
  | CurrentRoleFuncExprCommonSubexpr
  | CurrentUserFuncExprCommonSubexpr
  | SessionUserFuncExprCommonSubexpr
  | UserFuncExprCommonSubexpr
  | CurrentCatalogFuncExprCommonSubexpr
  | CurrentSchemaFuncExprCommonSubexpr
  | forall a. (IsAExpr a) => CastFuncExprCommonSubexpr a Typename
  | ExtractFuncExprCommonSubexpr (Maybe ExtractList)
  | OverlayFuncExprCommonSubexpr OverlayList
  | PositionFuncExprCommonSubexpr (Maybe PositionList)
  | SubstringFuncExprCommonSubexpr (Maybe SubstrList)
  | forall a. (IsAExpr a) => TreatFuncExprCommonSubexpr a Typename
  | TrimFuncExprCommonSubexpr (Maybe TrimModifier) TrimList
  | forall a b. (IsAExpr a, IsAExpr b) => NullIfFuncExprCommonSubexpr a b
  | CoalesceFuncExprCommonSubexpr ExprList
  | GreatestFuncExprCommonSubexpr ExprList
  | LeastFuncExprCommonSubexpr ExprList

-- |
-- ==== References
-- @
-- extract_list:
--   | extract_arg FROM a_expr
--   | EMPTY
-- @
data ExtractList = forall a. (IsAExpr a) => ExtractList ExtractArg a

-- |
-- ==== References
-- @
-- overlay_list:
--   | a_expr overlay_placing substr_from substr_for
--   | a_expr overlay_placing substr_from
-- @
data OverlayList = forall a b c. (IsAExpr a, IsAExpr b, IsAExpr c) => OverlayList a b c (Maybe AExpr)

-- |
-- ==== References
-- @
-- position_list:
--   | b_expr IN_P b_expr
--   | EMPTY
-- @
data PositionList = PositionList BExpr BExpr

-- |
-- ==== References
-- @
-- substr_list:
--   | a_expr substr_from substr_for
--   | a_expr substr_for substr_from
--   | a_expr substr_from
--   | a_expr substr_for
--   | expr_list
--   | EMPTY
-- @
data SubstrList
  = forall a. (IsAExpr a) => ExprSubstrList a SubstrListFromFor
  | ExprListSubstrList ExprList

-- |
-- ==== References
-- @
--   | a_expr substr_from substr_for
--   | a_expr substr_for substr_from
--   | a_expr substr_from
--   | a_expr substr_for
-- @
data SubstrListFromFor
  = forall a b. (IsAExpr a, IsAExpr b) => FromForSubstrListFromFor a b
  | forall a b. (IsAExpr a, IsAExpr b) => ForFromSubstrListFromFor a b
  | forall a. (IsAExpr a) => FromSubstrListFromFor a
  | forall a. (IsAExpr a) => ForSubstrListFromFor a

-- |
-- ==== References
-- @
-- trim_list:
--   | a_expr FROM expr_list
--   | FROM expr_list
--   | expr_list
-- @
data TrimList
  = forall a. (IsAExpr a) => ExprFromExprListTrimList a ExprList
  | FromExprListTrimList ExprList
  | ExprListTrimList ExprList

-- |
-- ==== References
-- @
-- case_expr:
--   | CASE case_arg when_clause_list case_default END_P
-- @
data CaseExpr = CaseExpr (Maybe AExpr) WhenClauseList (Maybe AExpr)

-- |
-- ==== References
-- @
-- when_clause_list:
--   | when_clause
--   | when_clause_list when_clause
-- @
type WhenClauseList = NonEmpty WhenClause

-- |
-- ==== References
-- @
-- when_clause:
--   |  WHEN a_expr THEN a_expr
-- @
data WhenClause = forall a b. (IsAExpr a, IsAExpr b) => WhenClause a b

-- |
-- ==== References
-- @
-- func_application:
--   |  func_name '(' ')'
--   |  func_name '(' func_arg_list opt_sort_clause ')'
--   |  func_name '(' VARIADIC func_arg_expr opt_sort_clause ')'
--   |  func_name '(' func_arg_list ',' VARIADIC func_arg_expr opt_sort_clause ')'
--   |  func_name '(' ALL func_arg_list opt_sort_clause ')'
--   |  func_name '(' DISTINCT func_arg_list opt_sort_clause ')'
--   |  func_name '(' '*' ')'
-- @
data FuncApplication = FuncApplication FuncName (Maybe FuncApplicationParams)

-- |
-- ==== References
-- @
-- func_application:
--   |  func_name '(' ')'
--   |  func_name '(' func_arg_list opt_sort_clause ')'
--   |  func_name '(' VARIADIC func_arg_expr opt_sort_clause ')'
--   |  func_name '(' func_arg_list ',' VARIADIC func_arg_expr opt_sort_clause ')'
--   |  func_name '(' ALL func_arg_list opt_sort_clause ')'
--   |  func_name '(' DISTINCT func_arg_list opt_sort_clause ')'
--   |  func_name '(' '*' ')'
-- @
data FuncApplicationParams
  = NormalFuncApplicationParams (Maybe Bool) (NonEmpty FuncArgExpr) (Maybe SortClause)
  | VariadicFuncApplicationParams (Maybe (NonEmpty FuncArgExpr)) FuncArgExpr (Maybe SortClause)
  | StarFuncApplicationParams

data FuncArgExpr
  = ExprFuncArgExpr AExpr
  | forall a. (IsAExpr a) => ColonEqualsFuncArgExpr Ident a
  | forall a. (IsAExpr a) => EqualsGreaterFuncArgExpr Ident a

class IsFuncArgExpr a where toFuncArgExpr :: a -> FuncArgExpr
instance IsFuncArgExpr FuncArgExpr where toFuncArgExpr = id
instance IsFuncArgExpr AExpr where toFuncArgExpr = ExprFuncArgExpr
instance IsFuncArgExpr CExpr where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr Columnref where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr AexprConst where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr Param where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr CaseExpr where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr FuncExpr where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr (SelectWithParens rows) where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr ArrayExpr where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr ExplicitRow where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr ImplicitRow where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr Iconst where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr Fconst where toFuncArgExpr = toFuncArgExpr . toAExpr
instance IsFuncArgExpr Bool where toFuncArgExpr = toFuncArgExpr . toAExpr

paramFuncArgExpr :: Param -> FuncArgExpr
paramFuncArgExpr = ExprFuncArgExpr . CExprAExpr . ParamCExpr

-- * Constants

-- |
-- AexprConst:
--   |  Iconst
--   |  FCONST
--   |  Sconst
--   |  BCONST
--   |  XCONST
--   |  func_name Sconst
--   |  func_name '(' func_arg_list opt_sort_clause ')' Sconst
--   |  ConstTypename Sconst
--   |  ConstInterval Sconst opt_interval
--   |  ConstInterval '(' Iconst ')' Sconst
--   |  TRUE_P
--   |  FALSE_P
--   |  NULL_P
data AexprConst
  = IAexprConst Iconst
  | FAexprConst Fconst
  | SAexprConst Sconst
  | BAexprConst Bconst
  | XAexprConst Xconst
  | FuncAexprConst FuncName (Maybe FuncConstArgs) Sconst
  | ConstTypenameAexprConst ConstTypename Sconst
  | StringIntervalAexprConst Sconst (Maybe Interval)
  | IntIntervalAexprConst Iconst Sconst
  | BoolAexprConst Bool
  | NullAexprConst

class IsAexprConst a where toAexprConst :: a -> AexprConst
instance IsAexprConst AexprConst where toAexprConst = id
instance IsAexprConst Iconst where toAexprConst = IAexprConst
instance IsAexprConst Fconst where toAexprConst = FAexprConst
instance IsAexprConst Bool where toAexprConst = BoolAexprConst

-- |
-- ==== References
-- @
--   |  func_name '(' func_arg_list opt_sort_clause ')' Sconst
-- @
data FuncConstArgs = FuncConstArgs (NonEmpty FuncArgExpr) (Maybe SortClause)

-- |
-- ==== References
-- @
-- ConstTypename:
--   | Numeric
--   | ConstBit
--   | ConstCharacter
--   | ConstDatetime
-- @
data ConstTypename
  = NumericConstTypename Numeric
  | ConstBitConstTypename ConstBit
  | ConstCharacterConstTypename ConstCharacter
  | ConstDatetimeConstTypename ConstDatetime

-- |
-- ==== References
-- @
-- Numeric:
--   | INT_P
--   | INTEGER
--   | SMALLINT
--   | BIGINT
--   | REAL
--   | FLOAT_P opt_float
--   | DOUBLE_P PRECISION
--   | DECIMAL_P opt_type_modifiers
--   | DEC opt_type_modifiers
--   | NUMERIC opt_type_modifiers
--   | BOOLEAN_P
-- opt_float:
--   | '(' Iconst ')'
--   | EMPTY
-- opt_type_modifiers:
--   | '(' expr_list ')'
--   | EMPTY
-- @
data Numeric
  = IntNumeric
  | IntegerNumeric
  | SmallintNumeric
  | BigintNumeric
  | RealNumeric
  | FloatNumeric (Maybe Int64)
  | DoublePrecisionNumeric
  | DecimalNumeric (Maybe TypeModifiers)
  | DecNumeric (Maybe TypeModifiers)
  | NumericNumeric (Maybe TypeModifiers)
  | BooleanNumeric

-- |
-- ==== References
-- @
-- Bit:
--   | BitWithLength
--   | BitWithoutLength
-- ConstBit:
--   | BitWithLength
--   | BitWithoutLength
-- BitWithLength:
--   | BIT opt_varying '(' expr_list ')'
-- BitWithoutLength:
--   | BIT opt_varying
-- @
data Bit = Bit OptVarying (Maybe ExprList)

type ConstBit = Bit

-- * Names & References

-- |
-- ==== References
-- @
-- columnref:
--   | ColId
--   | ColId indirection
-- @
data Columnref = Columnref ColId (Maybe Indirection)

-- |
-- ==== References
-- @
-- func_name:
--   | type_function_name
--   | ColId indirection
-- @
data FuncName
  = TypeFuncName TypeFunctionName
  | IndirectedFuncName ColId Indirection

instance IsString FuncName where
  fromString = TypeFuncName . fromString

-- |
-- ==== References
-- @
-- columnref:
--   | ColId
--   | ColId indirection
-- qualified_name:
--   | ColId
--   | ColId indirection
-- @
data QualifiedName
  = SimpleQualifiedName Ident
  | IndirectedQualifiedName Ident Indirection

-- |
-- ==== References
-- @
-- indirection:
--   |  indirection_el
--   |  indirection indirection_el
-- @
type Indirection = NonEmpty IndirectionEl

-- |
-- ==== References
-- @
-- indirection_el:
--   |  '.' attr_name
--   |  '.' '*'
--   |  '[' a_expr ']'
--   |  '[' opt_slice_bound ':' opt_slice_bound ']'
-- opt_slice_bound:
--   |  a_expr
--   |  EMPTY
-- @
data IndirectionEl
  = AttrNameIndirectionEl Ident
  | AllIndirectionEl
  | ExprIndirectionEl AExpr
  | SliceIndirectionEl (Maybe AExpr) (Maybe AExpr)

class IsIndirectionEl a where toIndirectionEl :: a -> IndirectionEl
instance IsIndirectionEl IndirectionEl where toIndirectionEl = id
instance IsIndirectionEl Ident where toIndirectionEl = AttrNameIndirectionEl
instance IsIndirectionEl String where toIndirectionEl = AttrNameIndirectionEl . fromString
instance IsIndirectionEl AExpr where toIndirectionEl = ExprIndirectionEl
instance IsIndirectionEl CExpr where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl Columnref where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl AexprConst where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl Param where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl CaseExpr where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl FuncExpr where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl (SelectWithParens rows) where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl ArrayExpr where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl ExplicitRow where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl ImplicitRow where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl Iconst where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl Fconst where toIndirectionEl = toIndirectionEl . toAExpr
instance IsIndirectionEl Bool where toIndirectionEl = toIndirectionEl . toAExpr

-- * Types

-- |
-- Typename definition extended with custom question-marks for nullability specification.
--
-- To match the standard Postgres syntax simply interpret their presence as a parsing error.
--
-- ==== References
-- @
-- Typename:
--   | SimpleTypename opt_array_bounds
--   | SETOF SimpleTypename opt_array_bounds
--   | SimpleTypename ARRAY '[' Iconst ']'
--   | SETOF SimpleTypename ARRAY '[' Iconst ']'
--   | SimpleTypename ARRAY
--   | SETOF SimpleTypename ARRAY
-- @
data Typename
  = Typename
      Bool
      -- ^ SETOF
      SimpleTypename
      Bool
      -- ^ Question mark
      (Maybe (TypenameArrayDimensions, Bool))
      -- ^ Array dimensions possibly followed by a question mark

-- |
-- ==== References
-- @
-- SimpleTypename:
--   | GenericType
--   | Numeric
--   | Bit
--   | Character
--   | ConstDatetime
--   | ConstInterval opt_interval
--   | ConstInterval '(' Iconst ')'
-- ConstInterval:
--   | INTERVAL
-- @
data SimpleTypename
  = GenericTypeSimpleTypename GenericType
  | NumericSimpleTypename Numeric
  | BitSimpleTypename Bit
  | CharacterSimpleTypename Character
  | ConstDatetimeSimpleTypename ConstDatetime
  | ConstIntervalSimpleTypename (Either (Maybe Interval) Iconst)

-- |
-- ==== References
-- @
-- GenericType:
--   | type_function_name opt_type_modifiers
--   | type_function_name attrs opt_type_modifiers
-- @
data GenericType = GenericType TypeFunctionName (Maybe Attrs) (Maybe TypeModifiers)

-- |
-- ==== References
-- @
-- opt_type_modifiers:
--   | '(' expr_list ')'
--   | EMPTY
-- @
type TypeModifiers = ExprList

-- |
-- ==== References
-- @
-- type_list:
--   | Typename
--   | type_list ',' Typename
-- @
type TypeList = NonEmpty Typename

-- * Operators

-- |
-- ==== References
-- @
--   | a_expr IS NULL_P
--   | a_expr IS TRUE_P
--   | a_expr IS FALSE_P
--   | a_expr IS UNKNOWN
--   | a_expr IS DISTINCT FROM a_expr
--   | a_expr IS OF '(' type_list ')'
--   | a_expr BETWEEN opt_asymmetric b_expr AND a_expr
--   | a_expr BETWEEN SYMMETRIC b_expr AND a_expr
--   | a_expr IN_P in_expr
--   | a_expr IS DOCUMENT_P
-- @
data AExprReversableOp
  = NullAExprReversableOp
  | TrueAExprReversableOp
  | FalseAExprReversableOp
  | UnknownAExprReversableOp
  | forall a. (IsAExpr a) => DistinctFromAExprReversableOp a
  | OfAExprReversableOp TypeList
  | forall a. (IsAExpr a) => BetweenAExprReversableOp Bool BExpr a
  | forall a. (IsAExpr a) => BetweenSymmetricAExprReversableOp BExpr a
  | InAExprReversableOp InExpr
  | DocumentAExprReversableOp

-- |
-- ==== References
-- @
--   | b_expr IS DISTINCT FROM b_expr
--   | b_expr IS NOT DISTINCT FROM b_expr
--   | b_expr IS OF '(' type_list ')'
--   | b_expr IS NOT OF '(' type_list ')'
--   | b_expr IS DOCUMENT_P
--   | b_expr IS NOT DOCUMENT_P
-- @
data BExprIsOp
  = DistinctFromBExprIsOp BExpr
  | OfBExprIsOp TypeList
  | DocumentBExprIsOp

-- * Indexes

-- |
-- ==== References
-- @
-- index_params:
--   | index_elem
--   | index_params ',' index_elem
-- @
type IndexParams = NonEmpty IndexElem

-- |
-- ==== References
-- @
-- index_elem:
--   | ColId opt_collate opt_class opt_asc_desc opt_nulls_order
--   | func_expr_windowless opt_collate opt_class opt_asc_desc opt_nulls_order
--   | '(' a_expr ')' opt_collate opt_class opt_asc_desc opt_nulls_order
-- @
data IndexElem = forall a. (IsIndexElemDef a) => IndexElem a (Maybe Collate) (Maybe Class) (Maybe AscDesc) (Maybe NullsOrder)

-- |
-- ==== References
-- @
--   | ColId opt_collate opt_class opt_asc_desc opt_nulls_order
--   | func_expr_windowless opt_collate opt_class opt_asc_desc opt_nulls_order
--   | '(' a_expr ')' opt_collate opt_class opt_asc_desc opt_nulls_order
-- @
data IndexElemDef
  = IdIndexElemDef ColId
  | FuncIndexElemDef FuncExprWindowless
  | ExprIndexElemDef AExpr

class IsIndexElemDef a where toIndexElemDef :: a -> IndexElemDef
instance IsIndexElemDef IndexElemDef where toIndexElemDef = id
instance IsIndexElemDef ColId where toIndexElemDef = IdIndexElemDef
instance IsIndexElemDef FuncExprWindowless where toIndexElemDef = FuncIndexElemDef
instance IsIndexElemDef AExpr where toIndexElemDef = ExprIndexElemDef
instance IsIndexElemDef CExpr where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef Columnref where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef AexprConst where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef Param where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef CaseExpr where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef FuncExpr where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef (SelectWithParens rows) where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef ArrayExpr where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef ExplicitRow where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef ImplicitRow where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef Iconst where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef Fconst where toIndexElemDef = toIndexElemDef . toAExpr
instance IsIndexElemDef Bool where toIndexElemDef = toIndexElemDef . toAExpr

instance IsString Ident where
  fromString = UnquotedIdent . fromString
