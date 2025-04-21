// Simulation Code - disabling and reenabling previously clicked on options

Qualtrics.SurveyEngine.addOnload(function() {
    var options = document.querySelectorAll('input[type="radio"]');
    var selectedOption = null; 
    var resetTimer = null; 

    // Store original label text for each option
    var originalLabels = {};
    options.forEach(function(option) {
        var label = document.querySelector('label[for="' + option.id + '"]');
        if (label) {
            originalLabels[option.id] = label.innerText;
        }
    });

    // Add click event listener to each option
    options.forEach(function(option) {
        option.addEventListener('click', function() {
            if (selectedOption && selectedOption !== option) {
                var oldLabel = document.querySelector('label[for="' + selectedOption.id + '"]');
                if (oldLabel) {
                    oldLabel.innerText = "No Ammo: Reloading"; 
                    oldLabel.style.color = "red"; 
                }

                // Disable previously selected option
                selectedOption.disabled = true;
            }

            // Update currently selected option
            selectedOption = option;

            // Update label text immediately when clicked for the current selection
            var currentLabel = document.querySelector('label[for="' + option.id + '"]');
            if (currentLabel) {
                currentLabel.innerText = "Firing"; 
                currentLabel.style.color = "green"; 
            }

            option.disabled = true;

            // If there's an existing reset timer, clear it
            if (resetTimer) {
                clearTimeout(resetTimer);
            }

            // Set a timer to reset all options after 20 seconds
            resetTimer = setTimeout(function() {
                // Loop through all options to reset the label text and re-enable the previous selections
                options.forEach(function(opt) {
                    var resetLabel = document.querySelector('label[for="' + opt.id + '"]');
                    if (resetLabel) {
                        resetLabel.innerText = originalLabels[opt.id];
                        resetLabel.style.color = "";
                    }

                    // Re-enable the previously selected options
                    opt.disabled = false;
                });

                // If there's a selected option, reset it as well
                if (selectedOption) {
                    var selectedLabel = document.querySelector('label[for="' + selectedOption.id + '"]');
                    if (selectedLabel) {
                        selectedLabel.innerText = originalLabels[selectedOption.id];
                        selectedLabel.style.color = "";
                    }

                    // Deselect the selected option after the 20-second timer
                    selectedOption.checked = false; // Deselect the current option
                    selectedOption.disabled = false; // Re-enable the currently selected option
                }

            }, 20000); // 20 second timeout
        });
    });
});




// Simulation Code: ZOMBIE ATTACK BLOCK - No mid-attack Changes

Qualtrics.SurveyEngine.addOnload(function() {
    var options = document.querySelectorAll('input[type="radio"]');
    var selectedOption = null; 

    // Store original label text for each option
    var originalLabels = {};
    options.forEach(function(option) {
        var label = document.querySelector('label[for="' + option.id + '"]');
        if (label) {
            originalLabels[option.id] = label.innerText;
        }
    });

    // Add click event listener to each option
    options.forEach(function(option) {
        option.addEventListener('click', function() {
            if (!selectedOption) {  // Allow selection only if none has been made
                selectedOption = option;

                // Update label text for selected option
                var currentLabel = document.querySelector('label[for="' + option.id + '"]');
                if (currentLabel) {
                    currentLabel.innerText = "Firing"; 
                    currentLabel.style.color = "green"; 
                }

                option.disabled = true; // Disable selected option to prevent re-clicking

                // Disable all other options and mark them as "No Mid-Attack Changes"
                options.forEach(function(opt) {
                    if (opt !== selectedOption) {
                        opt.disabled = true;
                        var otherLabel = document.querySelector('label[for="' + opt.id + '"]');
                        if (otherLabel) {
                            otherLabel.innerText = "No Mid-Attack Changes"; 
                            otherLabel.style.color = "red"; 
                        }
                    }
                });
            }
        });
    });
});

