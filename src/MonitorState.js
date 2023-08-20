
// General
export const preloadMaterials = m => () => m.preload()

// Mapping Materials
export const mapVidToMatNone = m => vt => () => m.materials.None.map = vt;

export const mapChildrenToMatNone = o => m => () => o.children[0].material = m.materials.None;

// Opacity
export const matTransparency = o => () => o.children[0].material.transparent = true;

export const matOpacity = o => n => () => o.children[0].material.opacity = n;

// Color
export const matColor = o => n1 => n2 => n3 => () => o.children[0].material.color = {r: n1, g: n2, b: n3};

// Emission Color
export const matEmisColour = o => n1 => n2 => n3 => () => o.children[0].material.emissive = {r: n1, g: n2, b: n3};

export const matEmisInt = o => n => () => o.children[0].material.emissiveIntensity = n

// Rotation
export const dynRotX = o => v => () => o.rotation.x += v;

export const dynRotY = o => v => () => o.rotation.y += v;

export const dynRotZ = o => v => () => o.rotation.z += v;
