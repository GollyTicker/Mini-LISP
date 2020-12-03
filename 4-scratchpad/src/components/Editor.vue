<template>
  <div class="box">
    <div class="row flex-fixed">
      <p class="sub-title" >Editor</p>
    </div>
    <div class="row flex-floating">
      <textarea id="#edit-text" v-model="value"/>
    </div>
  </div>
</template>

<script>
const default_lisp = `; basics
(define! test-var
  'test-value)
(define! ifelse '(lambda (b x y) (cond (b x) ('t y))))
(define! U-comb '(lambda (f x) (f f x)))
(define! null '(lambda (x) (eq x '())) )
(define! not '(lambda (x) (ifelse x '() 't)) )
(define! and '(lambda (a b) (ifelse a (ifelse b 't '()) '())) )
(define! or '(lambda (a b) (not (and (not a) (not b)))))`

export default {
  name: 'Editor',
  data: () => {
    return {
      value: default_lisp
    }
  }
}
</script>
