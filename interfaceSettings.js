// interfaceSettings.js

// 1. Load the Help Menu from helpMenu.html
document.addEventListener("DOMContentLoaded", async () => {
  const container = document.getElementById("helpContainer");
  if (container) {
    try {
      const response = await fetch("helpMenu.html");
      const html = await response.text();
      container.innerHTML = html;

      // Once loaded, initialize UI logic
      initializeHelpMenu();
      initializeEditorSettings();
    } catch (err) {
      console.error("Error loading helpMenu.html:", err);
    }
  }

  // 4. Keyboard shortcut: Shift + Enter to evaluate
  document.addEventListener('keydown', function (e) {
    if (e.shiftKey && e.key === "Enter") {
      e.preventDefault();
      window.doEval?.();
    }
  });
});

// 2. Help Menu navigation logic
function initializeHelpMenu() {
  const helpBtn = document.getElementById('helpButton');
  const helpPopup = document.getElementById('helpPopup');
  const closeHelp = document.getElementById('closeHelp');
  const navButtons = document.querySelectorAll('.helpNav button');
  const helpPages = document.querySelectorAll('.helpPage');

  navButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      const targetId = btn.getAttribute('data-page');
      helpPages.forEach(page => page.classList.add('hidden'));
      document.getElementById(targetId)?.classList.remove('hidden');
    });
  });

  helpBtn?.addEventListener('click', () => {
    helpPopup?.classList.toggle('hidden');
  });

  closeHelp?.addEventListener('click', () => {
    helpPopup?.classList.add('hidden');
  });
}

// 3. Editor settings: font, alignment, color

function initializeEditorSettings() {
  const root = document.documentElement;

  document.getElementById("fontSelect")?.addEventListener("change", (e) => {
    const font = e.target.value;
    const fontMap = {
      default: "monospace, 'Inconsolata'",
      acuarela: "'Acuarela'",
      acuarelX: "'acuarelX'",
      alargada: "'Alargada'",
      alargadX: "'AlargadX'",
      anarquia: "'Anarquia'",
      anarquiX: "'AnarquiX'",
      brillante: "'Brillante'",
      brillantX: "'BrillantX'",
      cinematica: "'Cinematica'",
      cinematicX: "'CinematicX'",
      escolar: "'Escolar'",
      escolXr: "'EscolXr'",
      estampa: "'Estampa'",
      estampX: "'EstampX'",
      molde: "'Molde'",
      moldX: "'MoldX'",
      nitida: "'Nitida'",
      nitidX: "'NitidX'",
      nocturna: "'Nocturna'",
      nocturnX: "'NocturnX'",
      plumon: "'Plumon'",
      plumXn: "'PlumXn'",
      salpicon: "'Salpicon'",
      salpicXn: "'SalpicXn'",
      treintas: "'Treintas'",
      treintXs: "'TreintXs'",
      voyeur: "'Voyeur'",
      voyeXr: "'VoyeXr'"
    };

    const cssFont = fontMap[font] || "Inconsolata, monospace";
    root.style.setProperty('--editor-font', cssFont);
  });

  document.getElementById("fontSizeSelect")?.addEventListener("change", (e) => {
    const size = e.target.value;
    root.style.setProperty('--editor-size', size);
  });

  document.getElementById("alignSelect")?.addEventListener("change", (e) => {
    root.style.setProperty('--editor-align', e.target.value);
  });

  document.getElementById("bgColorSelect")?.addEventListener("change", (e) => {
    root.style.setProperty('--editor-bg', e.target.value);
  });

  document.getElementById("fontColorSelect")?.addEventListener("change", (e) => {
    root.style.setProperty('--editor-color', e.target.value);
  });
}
