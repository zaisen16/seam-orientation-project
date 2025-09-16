# Anayzing Seam Orientation & Pitch Movement Using TrackMan 3D Spin Data

_Utilizing TrackMan 3D spin data to optimize & project pitch design through adjustments in seam orientation_

### Data Being Used

This project was all done using TrackMan data from Cape Cod League games in 2025. TrackMan provided all CCBL TrackMan units with their newer 3D spin camera, which captures the ball's orientation at release, generating useful information like each pitch's spin axis, orientation, spin efficiency, and more. 

## Main Goals

I used this data with the ultimate goal of identifying and projecting pitch design or pitch adjustments by changing a pitch's seam orientation. This is meant to be extremely actionable, showing how a pitcher can adjust a pitch's orientation and the effects it will have on pitch movement. Additionally, this simple and actionable result had to rely on several predictive models and conditional expectations(ICE). 

The main way I have visualized seam orientation is through a mollweide projection style plot of a pitcher's arsenal(example below):

<img width="726" height="338" alt="Screenshot 2025-08-11 at 11 22 13" src="https://github.com/user-attachments/assets/08350f8c-efb9-4081-8b6b-523cf0047629" />

This plot is a map of the baseball, with each point describing the location of spin axis.

Using several features of a pitch's spin & ball orientation, I used XGBoost to predict horizontal and induced vertical break. After creating a way to project pitch movement based only on the balls orientation and spin, I began to use this to identify if a pitcher can create a new pitch shape by only adjusting the seam orientation slightly. 

<img width="656" height="339" alt="Screenshot 2025-09-12 at 15 23 50" src="https://github.com/user-attachments/assets/38824506-b692-48f0-899f-89e24645e651" />

Above is what I have called a "sweep" style plot, about a pitcher's slider. This plot is showing the predicted horizontal break while "sweeping" through every value for the spin axis's horizontal/vertical point on the ball, using individualized conditional expectations(ICE). When you see the "cliffs" shown in the photo above, this indicates that making this slight adjustment in the seam orientation in this pitch will harness the seam effects to create much more movement.

In the case above, a pitcher with a tighter slider(averaging ~4" horz break) could change the seam orientation and add lots of horizontal break, bringing him closer to a sweeper shape. Below you can see what this seam orientation/axis change is like, and its potential results as predicted by the models:

<img width="804" height="305" alt="Screenshot 2025-09-16 at 12 50 30" src="https://github.com/user-attachments/assets/e6476481-6ef1-4285-be83-9210ca635e42" />

## Additional Reading

While the above does generally describe the goals and results of my project so far, it only scratches the surface of what is going on within these examples. For a much more in depth explanation of this project, please read the series of articles I have published on Medium:

[Part 1: Initial Research & Visualizing Seam Orientation](https://medium.com/@zachary.aisen/seam-orientation-analysis-part-1-initial-research-and-visuals-4176115982fb)

[Part 2: Modeling Pitch Movement with XGBoost](https://medium.com/@zachary.aisen/seam-orientation-analysis-part-2-modeling-pitch-movement-3e0f5173d377)

[Part 3: How to Identify & Test New Seam Orientations for Pitch Design](https://medium.com/@zachary.aisen/seam-orientation-analysis-part-3-how-to-identify-test-new-seam-orientations-6899609bdbde)

### Interested in my work?

Please feel free to reach out! My email is zachary.aisen@gmail.com, and you are also free to connect with me through the links to my Twitter or LinkedIn which are linked in my profile. 

