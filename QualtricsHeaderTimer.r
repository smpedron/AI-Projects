## Java script - Qualtrics - placing survey timer in header 'Look and Feel Tab'


<div id="inlineTimer" style="
  width: 100%;
  background: #f3f3f3;
  padding: 10px 16px;
  border-radius: 0 0 8px 8px;
  font-weight: bold;
  font-size: 16px;
  font-family: Arial, sans-serif;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  text-align: left;
  box-sizing: border-box;
  margin-bottom: 10px;">
  Time elapsed: <span id="timeDisplay">0:00</span>
    </div>
    
    <script>
    if (!window.surveyStartTime) {
      window.surveyStartTime = new Date();
    }
  
  function updateTimer() {
    const now = new Date();
    const elapsed = Math.floor((now - window.surveyStartTime) / 1000);
    const minutes = Math.floor(elapsed / 60);
    const seconds = elapsed % 60;
    const display = minutes + ":" + (seconds < 10 ? "0" : "") + seconds;
    const el = document.getElementById("timeDisplay");
    if (el) el.textContent = display;
  }
  
  setInterval(updateTimer, 1000);
  </script>
    