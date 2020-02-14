package com.github.maumay.tacocloud

import javax.validation.Constraint
import javax.validation.ConstraintValidator
import javax.validation.ConstraintValidatorContext
import javax.validation.Payload
import kotlin.reflect.KClass

@Target(AnnotationTarget.FIELD, AnnotationTarget.VALUE_PARAMETER)
@Retention(AnnotationRetention.RUNTIME)
@Constraint(validatedBy = [StateValidator::class])
annotation class IsState(
        val message: String = "Invalid state code",
        val groups: Array<KClass<*>> = [],
        val payload: Array<KClass<Payload>> = []
)


class StateValidator : ConstraintValidator<IsState, String>{
    companion object {
        private val states = setOf(
                "MA", "NY"
        )
    }

    override fun isValid(value: String?, context: ConstraintValidatorContext?): Boolean {
        return states.contains(value ?: "")
    }
}

