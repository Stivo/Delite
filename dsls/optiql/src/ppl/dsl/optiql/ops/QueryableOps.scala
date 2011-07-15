package ppl.dsl.optiql.ops

import ppl.dsl.optiql.datastruct.scala.container.{DataTable, Grouping}
import java.io.PrintWriter
import scala.virtualization.lms.common.{Base, ScalaGenFat, BaseFatExp}
import scala.virtualization.lms.internal.GenericFatCodegen
import ppl.dsl.optiql.OptiQLExp
import ppl.delite.framework.datastructures.FieldAccessOpsExp

trait QueryableOps extends Base {

  //type TransparentProxy[+T] = Rep[T]

  implicit def repToQueryableOps[TSource:Manifest](r: Rep[DataTable[TSource]]) = new QOpsCls(r) 
  implicit def repGroupingToQueryableOps[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = new QOpsCls(queryable_grouping_toDatatable(g))

  //override def __forward[A,B,C](self: TransparentProxy[A], method: String, x: TransparentProxy[B]*): TransparentProxy[C] = throw new RuntimeException("forwarding to " + method)

  class QOpsCls[TSource:Manifest](s: Rep[DataTable[TSource]]) {
    def Where(predicate: Rep[TSource] => Rep[Boolean]) = queryable_where(s, predicate)
	def GroupBy[TKey:Manifest](keySelector: Rep[TSource] => Rep[TKey]) = queryable_groupby(s,keySelector)
	def Select[TResult:Manifest](resultSelector: Rep[TSource] => Rep[TResult]) = queryable_select(s, resultSelector)
	def Sum(sumSelector: Rep[TSource] => Rep[Float]) = queryable_sum(s, sumSelector)
	def Average(avgSelector: Rep[TSource] => Rep[Float]) = queryable_average(s, avgSelector)
	def Count() = queryable_count(s)
  }
  
  //Grouping stuff
  def infix_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = queryable_grouping_key(g) 

  def queryable_where[TSource:Manifest](s: Rep[DataTable[TSource]], predicate: Rep[TSource] => Rep[Boolean]): Rep[DataTable[TSource]]
  def queryable_groupby[TSource:Manifest, TKey:Manifest](s: Rep[DataTable[TSource]], keySelector: Rep[TSource] => Rep[TKey]): Rep[DataTable[Grouping[TKey, TSource]]]
  def queryable_select[TSource:Manifest, TResult:Manifest](s: Rep[DataTable[TSource]], resultSelector: Rep[TSource] => Rep[TResult]): Rep[DataTable[TResult]]
  def queryable_sum[TSource:Manifest](s: Rep[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Float]): Rep[Float]
  def queryable_average[TSource:Manifest](s: Rep[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Float]): Rep[Float]
  def queryable_count[TSource:Manifest](s: Rep[DataTable[TSource]]): Rep[Int]  
  
  def queryable_grouping_toDatatable[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[DataTable[TSource]]
  def queryable_grouping_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[TKey]
  
  
}

trait QueryableOpsExp extends QueryableOps with BaseFatExp {
  this: QueryableOps with OptiQLExp =>

  case class QueryableWhere[TSource:Manifest](in: Exp[DataTable[TSource]], cond: Exp[TSource] => Exp[Boolean]) extends DeliteOpFilter[TSource, TSource,DataTable[TSource]] {
    def alloc = DataTable[TSource]()
    def func = e => e
    val size = in.size    
  }
     
  case class QueryableSelect[TSource:Manifest, TResult:Manifest](in: Exp[DataTable[TSource]], func: Exp[TSource] => Exp[TResult]) extends DeliteOpMap[TSource, TResult, DataTable[TResult]] {
	def alloc = DataTable[TResult](in.size)
    val size = in.size
  }
  
  case class QueryableGroupBy[TSource:Manifest, TKey:Manifest](s: Exp[DataTable[TSource]], v:Sym[TSource], key: Exp[TKey]) extends Def[DataTable[Grouping[TKey, TSource]]]    
  
  case class QueryableSum[TSource:Manifest](s: Exp[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Float]) extends DeliteOpLoop[Float] {
	val size = s.size
	//val v = fresh[Int]
	private[this] val rV = (fresh[Float], fresh[Float])
	val body: Def[Float] = DeliteReduceElem[Float](
		func = sumSelector(s(v)),
		zero = unit(0.0f),
		rV = rV,
		rFunc = rV._1 + rV._2
	)
  }
  case class QueryableAverage[TSource:Manifest](s: Exp[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Float]) extends Def[Float]
  
  //case class QueryableCount[TSource:Manifest](s: Exp[DataTable[TSource]]) extends Def[Int]

  case class QueryableGroupingToDataTable[TSource:Manifest, TKey:Manifest](g: Rep[Grouping[TKey, TSource]]) extends Def[DataTable[TSource]]
  case class QueryableGroupingKey[TSource:Manifest, TKey:Manifest](g: Rep[Grouping[TKey, TSource]]) extends Def[TKey]

  def queryable_select[TSource:Manifest, TResult:Manifest](s: Rep[DataTable[TSource]], resultSelector: Rep[TSource] => Rep[TResult]) = {
//	val v = fresh[TSource]
//	val func = reifyEffects(resultSelector(v))
	QueryableSelect(s, resultSelector)
  }
  
  def queryable_where[TSource:Manifest](s: Exp[DataTable[TSource]], predicate: Exp[TSource] => Exp[Boolean]) = QueryableWhere(s,predicate)
  def queryable_groupby[TSource:Manifest, TKey:Manifest](s: Exp[DataTable[TSource]], keySelector: Exp[TSource] => Exp[TKey]) = {
    val v = fresh[TSource]
    val key = keySelector(v)
    QueryableGroupBy(s, v, key)
  }
  def queryable_sum[TSource:Manifest](s: Rep[DataTable[TSource]], sumSelector: Rep[TSource] => Rep[Float]) = QueryableSum(s,sumSelector)
  def queryable_average[TSource:Manifest](s: Rep[DataTable[TSource]], avgSelector: Rep[TSource] => Rep[Float]) = toAtom(QueryableSum(s, avgSelector))/s.size()
  def queryable_count[TSource:Manifest](s: Rep[DataTable[TSource]]) = s.size()
  
  def queryable_grouping_toDatatable[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]) = QueryableGroupingToDataTable(g)
  def queryable_grouping_key[TKey:Manifest, TSource:Manifest](g: Rep[Grouping[TKey, TSource]]): Rep[TKey] = QueryableGroupingKey(g)
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case QueryableSum(s,ss) => queryable_sum(f(s), ss)
    case QueryableWhere(s,p) => queryable_where(f(s), p)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] //todo fix asInstanceOf
  
  override def syms(e: Any): List[Sym[Any]] = e match { 
    //case QueryableGroupBy(s,v,k) => syms(s) 
    case _ => super.syms(e)
  }
  
  override def boundSyms(e: Any): List[Sym[Any]] = e match {    
    case QueryableGroupBy(s,v,k) => v::syms(k)
	case _ => super.boundSyms(e)
  }  
  
}

trait ScalaGenQueryableOps extends ScalaGenFat {  
  val IR: QueryableOpsExp with OptiQLExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
	case QueryableGroupBy(s, v, k) =>  {
	  stream.println("val " + quote(sym) + " =  " + quote(s) + ".GroupBy( " + quote(v) + " => {")	  
	  emitBlock(k)	 
	  stream.println(quote(getBlockResult(k)) + "})")
	}
		
	case QueryableGroupingToDataTable(g) => emitValDef(sym, "generated.scala.container.DataTable.convertIterableToDataTable(" + quote(g) + ")")
	case QueryableGroupingKey(g) => emitValDef(sym, quote(g) + ".key")
    case _ => super.emitNode(sym,rhs)
  }
}
