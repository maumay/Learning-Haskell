package com.github.maumay.tacocloud

enum class IngredientType { WRAP, PROTEIN, VEGGIES, CHEESE, SAUCE }

data class Ingredient(val id: String, val name: String, val type: IngredientType)

