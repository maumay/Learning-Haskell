package com.github.maumay.tacocloud

import java.util.*

abstract class AbstractCezanneLayout {}

interface ICezanneShape<in L : AbstractCezanneLayout> {
    fun updateToFit(layout: L)
}

abstract class AbstractCezanneLayer<in L : AbstractCezanneLayout>(val key: String) {
    abstract fun shapeIterator(): Iterator<ICezanneShape<L>>

    fun updateShapesToFit(layout: L) = shapeIterator().forEach { it.updateToFit(layout) }
}

class LayerStack<L : AbstractCezanneLayout> (val layers: List<AbstractCezanneLayer<L>>) {
    inline fun <reified L0 : AbstractCezanneLayer<L>> getLayer(key: String): Optional<L0> {
        TODO()
    }
}