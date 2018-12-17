operator(prefix,  9, +, - );
operator(prefix,  9, ++, -- );
operator(postfix, 9, ++, -- );

operator(infixr, 6, / ); 
operator(infixr, 5, +, - );


class Addition(a, b, c) {
  inline (+)(x: a, y: b) c;
  const inline (+)(x: const a, y: const b): c;
}

instance Addition(I32, I32, I32) {
  (+)(x, y)
  {
    return __add(x, y);
  }
}

instance Addition(F32, F32, F32) {
  (+)(x, y)
  {
    return __addf(x, y);
  }
}

instance Addition(I32, F32, F32) {
  (+)(x, y)
  {
    return __addf(x as F32, y);
  }
}

instance Addition(F32, I32, F32) {
  (+)(x, y)
  {
    return __addf(x, y as F32);
  }
}

instance Addition(I32<>, I32<>, I32<> ) {
  (+)(x, y) {
    __add(*x, *y);
  }
}

instance Addition<n: Nat>(I32<n>, I32<n>, I32<n>)) {
  (+)(x, y) {
    __add(x, y);
  }
}

class Subtraction(a, b, c) {
  inline (-)(x: a, y: b): c;
}

class Multiplication(a, b, c) {
  inline (-)(x  a, y: b): c;
}

class Division(a, b, c) {
  inline (-)(x: a, y: b): c;
}



type Void();

alias Empty() = Void();

id<a>(x: a): a { return x; }

operator(infixr, 5, +, -);

extern print(str: *Char() ): Void A;

type Colors() { Red(); Green(); Blue(); }

type Array(x : Type) {
  Array
    { data: *x = null
    , size: I32 = 10
    , cap: I32 = 100
    , maxSize: const I32 = 10000
    }
}

type Node(x: Type){
  Node
    { data: x
    , next: *Node(x)
    }
}

Array()
  : data(null)
  , size(0)
  , cap(0)
{}

~Array()
{
  delete data;
}

Array(n: I32)
  : data(new I32[mul(n, 2)])
  , size(n)
  , cap(mul(n,2))
{}

Array ( other: &Array(x) )
  : data(new x[other.cap])
  , size(other.size)
  , cap(other.cap)
{
  for(int i = 0; i < size; ++i)
  {
    data[i] = other.data[i];
  }
}

Array ( n: I32, initial: &x )
  : data(new x[mul(n, 2)])
  , size(n)
  , cap(mul(n, 2))
{
  for (int i = 0; i < initialSize; ++i)
    data[i] = initial;
}

Array ( other: mut &&Array(x) )
  : data(other.data)
  , size(other.size)
  , cap(other.cap)
{
  other.data = null;
  other.size = 0;
  other.cap = 0;
}


Array ( data: ptr(x), size: Const(I32) )
  : data(x)
  , size(size)
  , capacity(size)
{
  for (i = 0; i < size; ++i)
  {
    this.data[i] = data[i];
  }
}

~Array()
{
  delete data;
}

append<x>(array: Array(x), elem: x)
{
  if (array.length >= array.capacity)
     array.resize(mul(capacity, 2));

  with (array) {
    data[size] = x;
    ++size;
  }
}


class GetLength(f) {
  getLength(item: &f): I32;
}

class SetLength(f) {
  setLength(thing: &f): Ref(f); 
}


inst GetLength<x>(Array(x))
{
  length(arr)
  {
    return arr.size;
  }
}

inst SetLength<x>(Array(x))
{
  length(arr: &Array(x), newLength: const I32): &Array(x)
  {
    // Newer allows arrays to resize intelligently.
    // Basically accepts a pointer and reallocs.
    // This will result in a new array.
    // Any given ptr might be invalidated.
    // If the resize is efficient, the array boundary will
    // simply grow.
    //newer arr.data [newLength];
    arr.size = newLength;
    arr.cap = newLength;
  }
}

class Sortable(f : Type -> Type)
{
  sort<a>( container: Ref(f(a)) ): Ref(f(a)) -> Ref(f(a)); 
}

inst Sortable<x>(Array(x))
{
  sort(container) // optional : Ref(Array(x))
  {
    /* Implement quicksort or something */
  }
}


doStuff<HasLength(a)>(thingy: a) {
  // do some stuff
  let len = getLength(thingy);
  // do more stuff
  return;
}


// Algebra!

class Category(c)
{
  compose(a: c, b: c): c;
}

class Semigroup(m)
{
  mappend(a: Const(m), b: Const(m)): m;
}

class Monoid <Semigroup(m)> (m)
{
  mempty(): m;
}

class Functor(f: Type -> Type)
{
  fmap<a,b>(f: REF(a) -> b, x: REF(f( Const(a) )) ): f(b);
}


// We can have a maybe type
type Maybe(x)
{
  Nothing();
  Just(x);
}


// And we can write Maybe to act algebraically.
inst Semigroup<Semigroup(x)>(Maybe(x))
{
  mappend(ma, mb)
  {
    case(ma) {
      Nothing() return b;
      Just(a) {
        case(mb) {
          Nothing() { return b;    }
          Just(b)   { return a<>b; }
        }
      }
    }
  }
}

inst Monoid<Monoid(x)>( Maybe(x) )
{
  mempty() { return Just(mempty()); }
}


// Functors are easily expressible
inst Functor(Maybe(x))
{
  fmap(f, m)
  {
    case(m) {
      Nothing()
        return Nothing();
      Just(x)
        return Just(f(x)); 
    }
  }
}

/* Simple main function */
main(): I32
{
  let foo([2, 3, 100, 4, 7], 5): I32[5];
  doStuff(foo);

  foo.sort();
  return 0;
}
