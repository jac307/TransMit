
// General

export const preloadMaterials = m => () => m.preload()

// Mapping Materials

// Material

export const mapVidToMatMat = m => vt => () => m.materials.Material.map = vt;

export const mapChildrenToMatMat = o => m => () => o.children[0].material = m.materials.Material;

// None

export const mapVidToMatNone = m => vt => () => m.materials.None.map = vt;

export const mapChildrenToMatNone0 = o => m => () => o.children[0].material = m.materials.None;

export const mapChildrenToMatNone1 = o => m => () => o.children[1].material = m.materials.None;

// Opacity


export const matTransparency0 = o => () => o.children[0].material.transparent = true;

export const matTransparency1 = o => () => o.children[1].material.transparent = true;

export const matOpacity0 = o => n => () => o.children[0].material.opacity = n;

export const matOpacity1 = o => n => () => o.children[1].material.opacity = n;


// object.children[0].material.transparent = true;
// object.children[0].material.opacity = 0.5;
