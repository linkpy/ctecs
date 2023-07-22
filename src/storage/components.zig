const std = @import("std");
const meta = @import("../shared/meta.zig");
const indices = @import("../shared/indices.zig");
const Descriptor = @import("../shared/Descriptor.zig");

const ArrayList = std.ArrayListUnmanaged;
const DynBitSet = std.DynamicBitSetUnmanaged;
const Allocator = std.mem.Allocator;
const EntityIndex = indices.EntityIndex;
const ComponentIndex = indices.ComponentIndex;

//#region ComponentMetadata

/// Per-component metadata used for `Added(C)` and `Changed(C)` query
/// filters.
pub const ComponentMetadata = struct {
	/// Tick when the component was added to the entity,
	added: u32 = 0,
	/// Tick of the last time the component was accessed mutably.
	changed: ?u32 = null,
};

//#endregion

//#region ComponentRow

/// Stores all of the information related to a specific component type.
pub fn ComponentRow(
	comptime C: type
) type {
	return struct {
		const Self = @This();

		/// Actual component entries.
		entries: ArrayList(C),
		/// Entity owner of each component entry.
		/// Always has the same length as `entries`.
		owners: ArrayList(EntityIndex),
		/// Component metadta of each component entry.
		/// Always has the same length as `entries`.
		metadata: ArrayList(ComponentMetadata),

		//#region init/deinit

		/// Initializes the component row.
		/// Note: This function assumes the row was defined as `undefined`.
		pub fn init(
			self: *Self
		) void {
			self.entries = .{};
			self.owners = .{};
			self.metadata = .{};
		}

		/// Initializes the component row with preallocated space.
		pub fn initCapacity(
			self: *Self,
			alloc: Allocator,
			num: usize
		) Allocator.Error!void {
			self.entries = try ArrayList(C).initCapacity(alloc, num);
			self.owners = try ArrayList(EntityIndex).initCapacity(alloc, num);
			self.metadata = try ArrayList(ComponentMetadata).initCapacity(alloc, num);
		}

		/// Deinitializes the component row.
		/// 
		/// Note: if `C` supports `deinit`, every component entry present will 
		/// be deinit'd.
		pub fn deinit(
			self: *Self,
			alloc: Allocator,
		) void {
			// execute the component's `deinit` if they have one.
			meta.maybeDeinit(self.entries.items, alloc);

			self.entries.deinit(alloc);
			self.owners.deinit(alloc);
			self.metadata.deinit(alloc);
		}

		//#endregion
	
		//#region entries access

		/// Gets the number of components present in the row.
		pub inline fn getLength(
			self: Self
		) usize {
			return self.entries.items.len;
		}

		pub inline fn getEntries(
			self: *Self
		) []C {
			return self.entries.items;
		}

		pub inline fn getEntriesConst(
			self: Self,
		) []const C {
			return self.entries.items;
		}

		pub inline fn getEntry(
			self: Self,
			index: ComponentIndex
		) C {
			return self.getEntriesConst()[@intFromEnum(index)];
		}

		pub inline fn getEntryPtr(
			self: *Self,
			index: ComponentIndex
		) *C {
			return &self.getEntries()[@intFromEnum(index)];
		}

		/// Sets the value of a component in the row.
		/// 
		/// Note: Does not update component metadata.
		pub inline fn setEntry(
			self: *Self,
			index: ComponentIndex,
			value: C
		) void {
			self.getEntries()[@intFromEnum(index)] = value;
		}

		//#endregion

		//#region owners access

		pub inline fn getOwners(
			self: *Self
		) []EntityIndex {
			return self.owners.items;
		}

		pub inline fn getOwnersConst(
			self: Self
		) []const EntityIndex {
			return self.owners.items;
		}

		pub inline fn getOnwer(
			self: Self,
			index: ComponentIndex,
		) EntityIndex {
			return self.getOwnersConst()[@intFromEnum(index)];
		}

		/// Sets the owner of a component in the row.
		/// 
		/// Note: Does not update the component metadata.
		pub inline fn setOwner(
			self: *Self,
			index: ComponentIndex,
			owner: EntityIndex
		) void {
			self.getOwners()[@intFromEnum(index)] = owner;
		}

		//#endregion

		//#region metadata access

		pub inline fn getMetadatas(
			self: *Self
		) []ComponentMetadata {
			return self.metadata.items;
		}

		pub inline fn getMetadatasConst(
			self: Self,
		) []const ComponentMetadata {
			return self.metadata.items;
		}

		pub inline fn getMetadata(
			self: Self,
			index: ComponentIndex
		) ComponentMetadata {
			return self.getMetadatasConst()[@intFromEnum(index)];
		}

		pub inline fn setMetadata(
			self: *Self,
			index: ComponentIndex,
			value: ComponentMetadata
		) void {
			self.getMetadatas()[@enumFromInt(index)] = value;
		}

		//#endregion

		//#region insertion/removal

		/// Inserts a new component at the end of the row.
		/// Allocates memory if the current capacity is reached.
		/// 
		/// Returns the index of the new component.
		pub fn insert(
			self: *Self,
			alloc: Allocator,
			data: C,
			owner: EntityIndex,
			added_tick: u32
		) Allocator.Error!ComponentIndex {
			try self.entries.append(alloc, data);
			// ensures that even in case of allocation error the row is still
			// in a valid state.
			errdefer _ = self.entries.pop();

			try self.owners.append(alloc, owner);
			errdefer _ = self.owners.pop();

			try self.metadata.append(alloc, .{
				.added = added_tick
			});

			const idx: u32 = @intCast(self.getLength() - 1);
			return @enumFromInt(idx);
		}

		/// Removes the component at the given index from the row.
		/// 
		/// Note: This function might change the index of another component
		/// in the row to keep components tightly packed in memory.
		/// 
		/// Returns the removed component data.
		pub fn remove(
			self: *Self,
			index: ComponentIndex
		) C {
			const idx = @intFromEnum(index);

			_ = self.owners.swapRemove(idx);
			_ = self.metadata.swapRemove(idx);
			return self.entries.swapRemove(idx);
		}

		//#endregion
	};
}

//#endregion

//#region ComponentStorage

/// Compile-time generated component storage. 
/// It contains a field per component type registered in the given `Descriptor`
/// of type `ComponentRow(C)`.
/// Do not use directly, use `ComponentStore` instead.
pub fn ComponentStorage(
	comptime desc: Descriptor
) type {
	var builder = meta.Struct {};

	for( desc.components, 0.. ) |Component, i| {
		const name = std.fmt.comptimePrint("{}", .{ i });
		builder.addField(name, ComponentRow(Component));
	}

	return @Type(builder.finish(false));
}

//#endregion

//#region ComponentStore

/// Usable component storage.
/// It wraps a `ComponentStorage` and provides functions to access the
/// component rows.
pub fn ComponentStore(
	comptime desc: Descriptor
) type {
	return struct {
		const Storage = ComponentStorage(desc);
		const Self = @This();

		/// Wrapped component storage.
		storage: Storage = undefined,

		//#region init/deinit

		/// Initializes the component store.
		/// Note: This function assumes the store was defined as `undefined`.
		pub fn init(
			self: *Self,
		) void {
			inline for( 0..desc.components.len ) |i| {
				self.getRowPtr(i).init();
			}
		}

		/// Initializes the compponent store with preallocated memory for each
		/// component row.
		pub fn initCapacity(
			self: *Self,
			alloc: Allocator,
			num: usize
		) Allocator.Error!void {
			inline for( 0..desc.components.len ) |i| {
				try self.getRowPtr(i).initCapacity(alloc, num);
			}
		}

		/// Deinitializes the component store.
		pub fn deinit(
			self: *Self,
			alloc: Allocator
		) void {
			inline for( 0..desc.components.len ) |i| {
				self.getRowPtr(i).deinit(alloc);
			}
		}

		//#endregion

		//#region Row access

		pub inline fn getRow(
			self: Self,
			comptime idx: usize
		) ComponentRow(desc.components[idx]) {
			const name = std.fmt.comptimePrint("{}", .{ idx });
			return @field(self.storage, name);
		}

		pub inline fn getRowPtr(
			self: *Self,
			comptime idx: usize
		) *ComponentRow(desc.components[idx]) {
			const name = std.fmt.comptimePrint("{}", .{ idx });
			return &@field(self.storage, name);
		}

		pub inline fn getRowOf(
			self: Self,
			comptime T: type
		) ComponentRow(T) {
			return self.getRow(desc.getComponentIndex(T));
		}

		pub inline fn getRowPtrOf(
			self: *Self,
			comptime T: type
		) *ComponentRow(T) {
			return self.getRowPtr(desc.getComponentIndex(T));
		}

		//#endregion

	};
}

//#endregion
