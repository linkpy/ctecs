const std = @import("std");
const meta = @import("meta.zig");

const EnumLiteral = meta.EnumLiteral;
const Descriptor = @This();

plugins: []const type = &.{},
components: []const type = &.{},
resources: []const type = &.{},
events: []const type = &.{},

archetypes: []const []const type = &.{},
stages: []const EnumLiteral = &.{},

//#region Creating descriptors

/// Creates a `Descriptor` from a descriptor structure.
/// 
/// The descriptor structure must be comptime and is defined as such :
/// 
/// ~~~~zig
/// 	const descriptor = .{
/// 		// Tuple of plugin structure types (optional)
/// 		.plugins = .{
/// 			PluginA, PluginB, ...
/// 		},
/// 		// Tuple of component types (optional)
/// 		.components = .{
/// 			ComponentA, ComponentB, ...
/// 		},
/// 		// Tuple of resource types (optional)
/// 		.resources = .{
/// 			ResourceA, ResourceB, ...
/// 		},
/// 		// Tuple of event types (optional)
/// 		.events = .{
/// 			EventA, EventB, ...
/// 		},
/// 		// Tuple of archetype (optional)
/// 		.archetypes = .{
/// 			// An archetype tuple is a type tuple with at least 2 types
/// 			// The types present in the archetypes has to also be present
/// 			// in the components tuple above.
/// 			.{ ComponentA, ComponentB },
/// 			.{ ComponentC, ComponentD, ... },
/// 		}
/// 		// Tuple of scheduling stage names (enum literals, optional)
/// 		.stages = .{
/// 			.stage_a, .stage_b, ...
/// 		}
/// 	};
/// ~~~~
/// 
/// All types are identified by a unique type identifier generated at compile time. 
/// It is allowed to defined an object multiple times. In that case only the first 
/// occurrence is kept.
pub fn from(
	comptime desc: anytype
) Descriptor {
	// validating input
	const Desc = @TypeOf(desc);
	const desc_info = @typeInfo(Desc);

	if( desc_info != .Struct ) 
		@compileError("A descriptor must be a compile-time tuple.");
	
	// creating and filling in the descriptor
	var res = Descriptor {};

	if( @hasField(Desc, "plugins") ) {
		for( desc.plugins ) |Plugin| {
			// note(estel): here we recursively import the full descriptor for
			// each plugin present. `fromPlugin` already asserts that `Plugin`
			// is a plugin structure type.
			res.import(fromPlugin(Plugin));
		}
	}

	if( @hasField(Desc, "components") ) {
		for( desc.components ) |Component| {
			assertTypeType(Component);
			res.addComponent(Component);
		}
	}

	if( @hasField(Desc, "resources") ) {
		for( desc.resources ) |Resource| {
			assertTypeType(Resource);
			res.addResource(Resource);
		}
	}

	if( @hasField(Desc, "events") ) {
		for( desc.events ) |Event| {
			assertTypeType(Event);
			res.addEvent(Event);
		}
	}

	if( @hasField(Desc, "archetypes") ) {
		for( desc.archetypes ) |archetype| {
			assertArchetypeTuple(archetype);
			const type_array = archetypeToTypeArray(archetype);
			res.addArchetype(type_array);
		}
	}

	if( @hasField(Desc, "stages") ) {
		for( desc.stages ) |stage| {
			assertEnumLiteral(stage);
			res.addStage(stage);
		}
	}

	// validating the resulting descriptor
	res.validateArchetypeCoherency();

	return res;
}

/// Creates a `Descriptor` from a plugin structure.
/// 
/// A plugin structure must follow the following example :
/// 
/// ~~~~zig
/// 	const ExamplePlugin = struct {
/// 		pub const descriptor = .{
/// 			// Descriptor as defined in `Descriptor.from`.
/// 		};
/// 	}
/// ~~~~
/// 
/// This functions simply assets the validity of the `Plugin` type,
/// then returns `Descriptor.from(Plugin.descriptor)`.
/// 
pub fn fromPlugin(
	comptime Plugin: type
) Descriptor {
	assertPluginType(Plugin);

	return from(Plugin.descriptor);
}

//#endregion

//#region Object presence checks within a descriptor

/// Checks if `Plugin` is in the list of plugins present in this descriptor.
fn hasPlugin(
	comptime self: Descriptor,
	comptime Plugin: type
) bool {
	return std.mem.indexOfScalar(type, self.plugins, Plugin) != null;
}

/// Checks if `Component` is in the list of components present in this descriptor.
fn hasComponent(
	comptime self: Descriptor,
	comptime Component: type
) bool {
	return std.mem.indexOfScalar(type, self.components, Component) != null;
}

/// Checks if `Resource` is in the list of resources present in this descriptor.
fn hasResource(
	comptime self: Descriptor,
	comptime Resource: type
) bool {
	return std.mem.indexOfScalar(type, self.resources, Resource) != null;
}

/// Checks if `Event` is in the list of events present in this descriptor.
fn hasEvent(
	comptime self: Descriptor,
	comptime Event: type
) bool {
	return std.mem.indexOfScalar(type, self.events, Event) != null;
}

/// Checks if `archetype` is in the list of archetypes present in this descriptor.
fn hasArchetype(
	comptime self: Descriptor,
	comptime archetype: []const type,
) bool {
	for( self.architypes ) |arch| {
		if( std.mem.eql(type, arch, archetype) )
			return true;
	}

	return false;
}

/// Checks if `stage` is in the list of archetypes present in this descriptor.
fn hasStage(
	comptime self: Descriptor,
	comptime stage: EnumLiteral,
) bool {
	return std.mem.indexOfScalar(EnumLiteral, self.stages, stage);
}

//#endregion

//#region Adding objects to a descriptor

/// Adds a plugin to the descriptor.
/// Doesnt add the plugin if it is already present in the descriptor.
/// 
/// Note: This function doesnt check if the plugin is a valid plugin before
/// adding it to the descriptor.
fn addPlugin(
	comptime self: *Descriptor,
	comptime Plugin: type
) void {
	if( !self.hasPlugin(Plugin) )
		self.plugins = self.plugins ++ &[_]type { Plugin };
}

/// Adds a component to the descriptor.
/// Doesnt add the component if it is already present in the descriptor.
fn addComponent(
	comptime self: *Descriptor,
	comptime Component: type
) void {
	if( !self.hasComponent(Component) )
		self.components = self.components ++ &[_]type { Component };
}

/// Adds a resource to the descriptor.
/// Doesnt add the resource if it is already present in the descriptor.
fn addResource(
	comptime self: *Descriptor,
	comptime Resource: type
) void {
	if( !self.hasResource(Resource) )
		self.resources = self.resources ++ &[_]type { Resource };
}

/// Adds an event to the descriptor.
/// Doesnt add the event if it is already present in the descriptor.
fn addEvent(
	comptime self: *Descriptor,
	comptime Event: type
) void {
	if( !self.hasEvent(Event) )
		self.events = self.events ++ &[_]type { Event };
}

/// Adds an archetype to the descriptor.
/// Doesnt add the archetype if it is already present in the descriptor.
fn addArchetype(
	comptime self: *Descriptor,
	comptime archetype: []const type,
) void {
	if( !self.hasArchetype(archetype) )
		self.archetypes = self.archetypes ++ &[_][]const type { archetype };
}

/// Adds a scheduling stage to the descriptor.
/// Doesnt add the stage if it is already present in the descriptor.
fn addStage(
	comptime self: *Descriptor,
	comptime stage: EnumLiteral,
) void {
	if( !self.hasStage(stage) )
		self.stages = self.stages ++ &[_]EnumLiteral { stage };
}

//#endregion

//#region Merging descriptors

/// Imports all of the objects present in the `other` descriptor into `self`.
/// Uses the `add*` methods to not add duplicates.
fn import(
	comptime self: *Descriptor,
	comptime other: Descriptor
) void {
	for( other.plugins ) |Plugin| 
		self.addPlugin(Plugin);
	
	for( other.components ) |Component|
		self.addComponent(Component);
	
	for( other.resources ) |Resource|
		self.addResource(Resource);

	for( other.events ) |Event|
		self.addEvent(Event);
	
	for( other.archetypes ) |archetype|
		self.addArchetype(archetype);
	
	for( other.stages ) |stage|
		self.addStage(stage);
}

//#endregion

//#region Validating descriptors

/// Validates the coherency of the archetypes. 
/// It ensures that all of the archetypes only contains registered components.
fn validateArchetypeCoherency(
	self: Descriptor
) void {
	for( self.archetypes ) |archetype| {
		for( archetype ) |Component| {
			if( !self.hasComponent(Component) ) {
				const err = std.fmt.comptimePrint(
					"The archetype `{any}` contains the component `{s}` which isnt declared in the descriptor.",
					.{ archetype, @typeName(Component) }
				);
				@compileError(err);
			}
		}
	}
}

//#endregion

//#region Type checks

/// Asserts that the given thing is a plugin type.
fn assertPluginType(
	comptime Plugin: anytype
) void {
	if( @TypeOf(Plugin) != type ) 
		@compileError("Only structure types are accepted as plugins, received a " ++ @typeName(@TypeOf(Plugin)));
	
	const info = @typeInfo(Plugin);

	if( info != .Struct )
		@compileError("Only structure types are accepted as plugins, recevied a " ++ @tagName(info) ++ " type (for " ++ @typeName(Plugin) ++ ")");
	
	if( !@hasDecl(Plugin, "descriptor") )
		@compileError("All plugin types must publicly expose a `descriptor` (for " ++ @typeName(Plugin) ++ ")");
}

/// Asserts that the given thing is a type.
fn assertTypeType(
	comptime Type: anytype
) void {
	if( @TypeOf(Type) != type )
		@compileError("Expected a type, received a " ++ @typeName(@TypeOf(Type)));
}

/// Asserts that the given thing is an archetype (type tuple).
fn assertArchetypeTuple(
	comptime archetype: anytype
) void {
	const Type = @TypeOf(archetype);
	const info = @typeInfo(Type);

	if( info != .Struct or !info.Struct.is_tuple )
		@compileError("Expected a type tuple, received a " ++ @tagName(info));
	
	for( info.Struct.fields ) |field| {
		if( field.type != type )
			@compileError("Expected a type tuple, but found a " ++ @typeName(field.type) ++ " inside.");
	}
}

/// Asserts that the given thing is an enum literal.
fn assertEnumLiteral(
	comptime literal: anytype
) void {
	if( @TypeOf(literal) != EnumLiteral )
		@compileError("Expected an enum literal, received a " ++ @typeName(@TypeOf(literal)));
}

//#endregion

//#region Conversions

/// Converts an archetype (type tuple) to a type array.
fn archetypeToTypeArray(
	comptime archetype: anytype
) []const type {
	var res: []const type = &.{};

	for( @typeInfo(archetype).Struct.fields ) |field| 
		res = res ++ @field(archetype, field.name);

	return res;
}

//#endregion
