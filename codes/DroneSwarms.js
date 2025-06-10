Qualtrics.SurveyEngine.addOnload(function() {
    function getRandomInt(min, max) {
        return Math.floor(Math.random() * (max - min + 1)) + min;
    }

    const trueZombieCounts = [60, 65, 70, 75, 80, 85, 90, 95, 100];
    const actualZombies = trueZombieCounts[Math.floor(Math.random() * trueZombieCounts.length)];

    const aLow = actualZombies - getRandomInt(4, 7);
    const aHigh = actualZombies + getRandomInt(4, 7);
    const swarmAEstimate = aLow + "–" + aHigh;

    const upwardBias = getRandomInt(3, 6);
    const bLow = actualZombies + upwardBias - getRandomInt(4, 7);
    const bHigh = actualZombies + upwardBias + getRandomInt(4, 7);
    const swarmBEstimate = bLow + "–" + bHigh;

    console.log("Actual Zombies:", actualZombies);
    console.log("Swarm A:", swarmAEstimate);
    console.log("Swarm B:", swarmBEstimate);

    // Delay execution slightly to make sure question text renders
    setTimeout(function() {
        var qContainer = this.getQuestionTextContainer ? this.getQuestionTextContainer() : null;

        if (qContainer) {
            var html = qContainer.innerHTML;
            html = html.replace("XXX1", swarmAEstimate);
            html = html.replace("XXX2", swarmBEstimate);
            qContainer.innerHTML = html;
            console.log("Updated HTML content:", html);
        } else {
            console.error("Could not find question text container.");
        }

        Qualtrics.SurveyEngine.setEmbeddedData("trueZombieCount", actualZombies);
        Qualtrics.SurveyEngine.setEmbeddedData("swarmA", swarmAEstimate);
        Qualtrics.SurveyEngine.setEmbeddedData("swarmB", swarmBEstimate);
    }.bind(this), 200); // .bind(this) so `this.getQuestionTextContainer()` still works
});
