const std = @import("std");
const bin = std.builtin;

pub const EnumLiteral = @Type(.EnumLiteral);

/// Utility used to create structures at compile time.
pub const Struct = struct {
	fields: []const bin.Type.StructField = &.{},

	/// Adds a field to the structure.
	pub fn addField(
		comptime self: *Struct,
		comptime name: []const u8,
		comptime Type: type
	) void {
		self.fields = self.fields ++ [1]bin.Type.StructField { .{
			.name = name,
			.type = Type,
			.default_value = null,
			.is_comptime = false,
			.alignment = @alignOf(Type)
		}};
	}

	/// Returns the type info of the structure.
	/// 
	/// Note: To get a type, do `@Type(builder.finish(...))`.
	pub fn finish(
		comptime self: Struct,
		comptime tuple: bool
	) bin.Type {
		return .{ .Struct = .{
			.layout = .Auto,
			.backing_integer = null,
			.fields = self.fields,
			.decls = &.{},
			.is_tuple = tuple,
		}};
	}
};

/// Utility used to create enums at compile-time.
pub const Enum = struct {
	fields: []const bin.Type.EnumField = &.{},
	next_value: comptime_int = 0,

	/// Adds a field to the enum.
	/// If `value` is null, the next available value will be used.
	pub fn addField(
		comptime self: *Enum,
		comptime name: []const u8,
		comptime value: ?comptime_int
	) void {
		self.fields = self.fields ++ [1]bin.Type.EnumField { .{
			.name = name,
			.value = value orelse self.next_value,
		}};

		if( value == null )
			self.next_value += 1;
	}

	/// Returns the type info of the enum.
	/// 
	/// Note: To get a type, do `@Type(builder.finish(...))`.
	pub fn finish(
		comptime self: Enum,
		comptime Tag: type,
		comptime exhaustive: bool,
	) bin.Type {
		return .{ .Enum = .{
			.tag_type = Tag,
			.fields = self.fields,
			.decls = &.{},
			.is_exhaustive = exhaustive,
		}};
	}
};

/// Utility used to create unions at compile-time.
pub const Union = struct {
	fields: []const bin.Type.UnionField = &.{},
	next_value: comptime_int = 0,

	/// Adds a field to the union.
	pub fn addField(
		comptime self: *Union,
		comptime name: []const u8,
		comptime Type: type
	) void {
		self.fields = self.fields ++ [1]bin.Type.UnionField { .{
			.name = name,
			.type = Type,
			.alignment = @alignOf(Type),
		}};
	}

	/// Returns the type info of the union.
	/// 
	/// Note: To get a type, do `@Type(builder.finish(...))`.
	pub fn finish(
		comptime self: Union,
		comptime Tag: ?type,
	) bin.Type {
		return .{ .Union = .{
			.layout = .Auto,
			.tag_type = Tag,
			.fields = self.fields,
			.decls = &.{},
		}};
	}
};

/// Checks the type of the given object and if a deinit method exists, it
/// will be called. Otherwise, nothing is done.
/// 
/// Two deinit topologies are supported :
/// - `deinit(*T)` 
/// - `deinit(*T, Allocator)`
/// 
/// Supports :
/// - `struct`, `enum`, `union`, `opaque` with a `deinit` function ;
/// - `*T`, `[]T` with `T` being a type supported by this function ;
/// 
/// Note: For pointers, only the deinit function is called, the memory isnt
/// released. It also doesnt work with pointers of pointers.
pub fn maybeDeinit(
	obj: anytype,
	alloc: std.mem.Allocator
) void {
	const T = @TypeOf(obj);
	const info = @typeInfo(T);

	if( comptime info != .Pointer )
		@compileError("Cannot deinit an immutable value.");
	
	const P = @TypeOf(info.Pointer.child);
	const p_info = @typeInfo(P);

	switch(comptime p_info) {
		.Struct,
		.Enum,
		.Union,
		.Opaque => {
			if( comptime !@hasDecl(P, "deinit") )
				return;
			
			if( comptime @typeInfo(@TypeOf(P.deinit)).Fn.params.len == 2 ) {
				switch(comptime info.Pointer.size) {
					.One => P.deinit(obj),
					.Slice => 
						for( obj ) |*o|
							P.deinit(o),
					else => @compileError("Only []T and *T are supported.")
				}

			} else {
				switch(comptime info.Pointer.size) {
					.One => P.deinit(obj, alloc),
					.Slice => 
						for( obj ) |*o|
							P.deinit(o, alloc),
					else => @compileError("Only []T and *T are supported.")
				}
			}
		}
	}
}
