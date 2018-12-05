type Void();

alias Empty() = Void();


infixr 5 + -;

extern "ccall" print(str: *Char() );

type Colors() { Red(); Green(); Blue(); }

type Array(x : Type) {
  Array(
    data: Ptr(x) = null,
    size: I32 = 10,
    cap: I32 = 100,
    maxSize: const I32 = 10000,
  );
}

// Immutable list, not that useful
type const Node(x: Type){
  data Node {
    data: x;
    next: Ptr(Node(x));
  }
}

Array()
  : data(null)
  , size(0)
  , cap(0)
{}

~Array()
  : data(null)
  , size(0)
  , cap(0)
{}

Array(n: I32)
  : data(new I32[n*2])
  , size(n)
  , cap(n*2)
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
  : data(new x[n*2])
  , size(n)
  , cap(n*2)
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
  delete(data, size);
}

append<x>(array: Array(x), elem: x)
{
  if (array.length >= array.capacity)
     array.resize(capacity*2);

  with (array) {
    data[size] = x;
    ++size;
  }
}


class GetLength(f) {
  getLength(item: REF(f)): I32;
}

class SetLength(f) {
  setLength(thing: Ref(f)): Ref(f); 
}


inst GetLength<x>(Array(x))
{
  length(arr: REF(Array(x)) -> I32
  {
    return arr.size;
  }
}

inst SetLength<x>(Array(x))
{
  length(arr: Ref(Array(x), newLength: Const(I32)): Ref(Array(x))
  {
    // Newer allows arrays to resize intelligently.
    // Basically accepts a pointer and reallocs.
    // This will result in a new array.
    // Any given ptr might be invalidated.
    // If the resize is efficient, the array boundary will
    // simply grow.
    newer arr.data [newLength];
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

class Monoid<Semigroup m>(m)
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
impl Semigroup<Semigroup x>(Maybe(x))
{
  mappend(a, b)
  {
    case(a, b) {
      Nothing(), _ {
        return b;
      Just(_), Nothing() {
        return a;
      }
      Just(x), Just(y) {
        return Just(x<>y);
      }
    }
  }
}

impl Monoid<Monoid x>(Maybe(x))
{
  mempty() { return Just(mempty()); }
}


// Functors are easily expressible
impl Functor(Maybe(x))
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
  let foo([2, 3, 100, 4, 7], 5): Arr(I32);
  doStuff(foo);

  foo.sort();
  return 0;
}
