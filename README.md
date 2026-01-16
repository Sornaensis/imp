![Imp DSL](doc/img/imp.png)

# The Imp DSL

This DSL describes an imperative core that elaborates to Haskell declarations and expressions. The semantics below are independent of surface syntax.

## Programs and modules

- A module is a sequence of **type declarations**, **capability declarations**, and **procedure/function definitions**.
- A statement block is a sequence of **statements** that executes left-to-right and returns the last expression (if any).

## Types

- **Primitive types**: `Bool`, `Int`, `Float`, `String`, `Text`.
- **Algebraic types**: `data` and `newtype` create tagged sum and product types with constructors.
- **Records**: record fields are named; lenses are generated for field access in the runtime layer.
- **Type constructors**: `Option a`, `List a`, `Map k v`.
- **Unit/Void**: `Unit` has a single value; `Void` has none.

## Expressions

- **Variables** resolve lexically.
- **Literals** evaluate to their obvious Haskell counterparts.
- **Tagged literals** invoke `taggedLiteral` at runtime.
- **Field access** reads through a lens or record selector.
- **Function application** is call-by-value.
- **Operators** are syntax sugar for named functions; they resolve to in-scope bindings.

## Statements

- **Let** binds a local name to the value of an expression.
- **Assign** updates a mutable reference (`Ref a`) via a lens or direct set.
- **If** evaluates the condition; executes exactly one branch.
- **While** repeats while the condition is true.
- **Return** exits a procedure early with a value.
- **Await** executes an action in the current monad (only allowed in `proc`).

## Functions vs procedures

- **`fn`** defines a *pure* function. Its body contains only pure statements and expressions. It elaborates to a Haskell function in `Identity`.
- **`proc`** defines an *effectful* procedure. Its body may use `await`, refs, and capabilities. It elaborates to a Haskell function in a monad with a `Monad` constraint.

## Capabilities

- A **capability** declares a set of methods (effectful operations) that can be required by procedures.
- For a capability `Cap`, the DSL generates:
	- `HasCap` typeclass with a method per capability operation.
	- `requireCap` helper for obtaining the capability dictionary.
	- Method names of the form `cap_<capLower>_<method>` to avoid clashes.

## Runtime values

- **Refs** model mutable state; `set` and `modify` operate through lenses.
- **Tagged literals** are decoded via `taggedLiteral` in scope.
- **Operators** resolve to Haskell functions in scope at the splice site.

## Validation rules (selected)

- Undefined variables, constructors, and capabilities are errors.
- `await` is only valid inside `proc`.
- Pure `fn` bodies cannot perform effectful statements.
- Capability methods must be called only in contexts that require the capability.

## Examples

### Types, capabilities, operators, functions, procedures

```haskell
[impModule|
	newtype UserId = int deriving (Eq);

	type User {
		userId: UserId;
		userName: Text;
		userEmail: Option<Text>;
	}

	enum Status { Active, Disabled }

	capability Logging {
		log(msg: Text): Unit;
	}

	operators {
		(++) = stringAppend;
	}

	fn greeting(name: Text): Text {
		let msg = "hello " ++ name;
		return msg;
	}

	proc logGreeting(name: Text): Unit requires (Logging) {
		await capabilities.Logging.log(greeting(name));
	}
|]
```

### Expressions, statements, refs, and control flow

```haskell
[imp|
	var count = 0;
	while (count < 3) {
		count = count + 1;
	}

	if (count == 3) {
		this.userName = "done";
	} else {
		this.userName = "pending";
	}

	return count;
|]
```

### Tagged literals and Haskell splices

```haskell
[imp|
	var uid = new UserId(userId#1);
	var email = email#"demo@example.com";
	var maybeUid = hs{ Just uid };
	return uid;
|]
```

### Pure helper in `fn`

```haskell
[impModule|
	fn makeUser(uid: UserId, name: Text): User {
		return new User(uid, name, null);
	}
|]
```
