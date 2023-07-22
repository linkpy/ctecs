
/// Index used to represent an entity in the ECS.
pub const EntityIndex = enum(u32) { _ };

/// Index used to represent a component in the ECS.
pub const ComponentIndex = enum(u32) { _ };

/// Structured used to represent an entity. Should be 
/// used whenever possible as it is safer than `EntityIndex` against
/// use-after-frees.
pub const Entity = packed struct {
	index: EntityIndex,
	generation: u32,
};
