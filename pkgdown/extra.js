document.addEventListener("DOMContentLoaded", function () {
  document.querySelectorAll("details.chunk-details").forEach(function (details) {
    const button = details.querySelector(":scope > summary");

    if (!button) return;

    function updateLabel() {
      button.textContent = details.open ? "Hide Code" : "Show Code";
    }

    updateLabel();
    details.addEventListener("toggle", updateLabel);
  });
});