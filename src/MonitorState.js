
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

// Color

export const matColor0 = o => n1 => n2 => n3 => () => o.children[0].material.color = {r: n1, g: n2, b: n3};

export const matColor1 = o => n1 => n2 => n3 => () => o.children[1].material.color = {r: n1, g: n2, b: n3};

// Emission Color

export const matEmisInt0 = o => n => () => o.children[0].material.emissiveIntensity = n

export const matEmisInt1 = o => n => () => o.children[1].material.emissiveIntensity = n

export const matEmisive0 = o => n1 => n2 => n3 => () => o.children[0].material.emissive = {r: n1, g: n2, b: n3};

export const matEmisive1 = o => n1 => n2 => n3 => () => o.children[1].material.emissive = {r: n1, g: n2, b: n3};


// object.children[0].material.transparent = true;
// object.children[0].material.opacity = 0.5;
