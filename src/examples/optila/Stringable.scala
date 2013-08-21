package ppl.dsl.forge
package examples
package optila

import core.{ForgeApplication,ForgeApplicationRunner}

/**
 * Defines the Stringable type class for scalars, vectors, and matrices.
 * This type class is necessary because the correct toString is not callable from within Delite.
 */
trait StringableOps {
  this: OptiLADSL =>

  object TStringable extends TypeClassSignature {
    def name = "Stringable"
    def prefix = "_str"
    def wrapper = Some("strtype")
  }

  def importStringableOps() {
    val T = tpePar("T")

    val Stringable = tpeClass("Stringable", TStringable, T)

    // Stringable type class interface
    infix (Stringable) ("makeStr", T, T :: MString)

    // primitive implementations
    val DoubleStringable = tpeClassInst("StringableDouble", Nil, Stringable(MDouble))
    infix (DoubleStringable) ("makeStr", Nil, MDouble :: MString) implements composite ${ "" + $0 }

    val FloatStringable = tpeClassInst("StringableFloat", Nil, Stringable(MFloat))
    infix (FloatStringable) ("makeStr", Nil, MFloat :: MString) implements composite ${ "" + $0 }

    val IntStringable = tpeClassInst("StringableInt", Nil, Stringable(MInt))
    infix (IntStringable) ("makeStr", Nil, MInt :: MString) implements composite ${ "" + $0 }

    val BoolStringable = tpeClassInst("StringableBool", Nil, Stringable(MBoolean))
    infix (BoolStringable) ("makeStr", Nil, MBoolean :: MString) implements composite ${ "" + $0 }

    val StrStringable = tpeClassInst("StringableStr", Nil, Stringable(MString))
    infix (StrStringable) ("makeStr", Nil, MString :: MString) implements composite ${ $0 }

    // OptiLA types
    val DenseVector = lookupTpe("DenseVector")
    val DenseVectorView = lookupTpe("DenseVectorView")
    val IndexVector = lookupTpe("IndexVector")
    val DenseMatrix = lookupTpe("DenseMatrix")

    val DenseVectorStringable = tpeClassInst("StringableDenseVector", T withBound TStringable, Stringable(DenseVector(T)))
    infix (DenseVectorStringable) ("makeStr", Nil, DenseVector(T) :: MString) implements composite ${ $0.makeString }

    val DenseVectorViewStringable = tpeClassInst("StringableDenseVectorView", T withBound TStringable, Stringable(DenseVectorView(T)))
    infix (DenseVectorViewStringable) ("makeStr", Nil, DenseVectorView(T) :: MString) implements composite ${ $0.makeString }

    val IndexVectorStringable = tpeClassInst("StringableIndexVector", Nil, Stringable(IndexVector))
    infix (IndexVectorStringable) ("makeStr", Nil, IndexVector :: MString) implements composite ${ $0.makeString }

    val DenseMatrixStringable = tpeClassInst("StringableDenseMatrix", T withBound TStringable, Stringable(DenseMatrix(T)))
    infix (DenseMatrixStringable) ("makeStr", Nil, DenseMatrix(T) :: MString) implements composite ${ $0.makeString }
  }
}