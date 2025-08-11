***Analyzing Seam Orientation Data***



**What is the data?**

I am using TrackMan data tracked using the 3D Spin camera, which has been attached to all Cape Cod League TrackMan units. The data captures periodically, so my dataset is not every pitch thrown in the cape this summer, but still has many.





**Initial Research & Visuals**

First, I needed to be able to actually visualize this data, and find out if there is any true relationship of seam orientation to movement. 
I started with creating function to visualize the location of a pitches' spin axis on the ball, essentially explaining seam orientation. This is done through a mollweide projection style plot(example below).
What is being plotting is the spin axis: if I were to put a metal rod through the spin axis of a 4-seam fastball to visualize the spin, this is plotting the entry & exit point of that rod. Each dot is a point at which the ball is rotating around, similar to how the Earth rotates around the north & south poles, for example. 
(Please note, that because the ball's orientation is ambiguous, each pitch actually has 4 points associated with it)


<img width="726" height="338" alt="Screenshot 2025-08-11 at 11 22 13" src="https://github.com/user-attachments/assets/b8db4d75-071f-4c2f-8e06-46260457decc" />


(This Pitcher's movement plot, for reference):
<img width="582" height="404" alt="Screenshot 2025-08-11 at 11 23 15" src="https://github.com/user-attachments/assets/1f692e43-b1ac-4793-88ff-44e2c913e5f4" />

To explain this pitcher's mollweide style plot:

He throws a fastball with a very typical 4-seam orientation, getting perfect 4-seam backspin.
His cutter is just offset from the 4-seam fastball's orientation.
And his slider follows a very typical sweeper orientation, with the ball rotating around that bottom right/top left part of the seam. Pitchers can also recognize this as having a "red dot" on the ball, from the ball rotating directly around the seam, creating a dot on the ball visually. It can also be referred to as a more "one-seam" look, with one solid line from the seam as it is spinning.
(More on this orientation later)


**Can Seam Orientation Relate to Movement?**
At this point, I wanted to see if there is any actual evidence of a certain seam orientation causing more movement(ie, detecting seam shifted wake effects).
The first place to start with this is analyzing sliders(and sweepers). It is well documented that the sweeper is picking up so much horizontal break due to the seam effects of the pitch. 
Here is a mollweide plot of all righty sliders(colored to horizontal break, the darker points having the most sweep):

<img width="1062" height="517" alt="Screenshot 2025-08-09 at 21 21 39" src="https://github.com/user-attachments/assets/536e4417-e632-4c13-9f4c-668ddd26464b" />

Clearly there is a certain seam orientation required to throw a sweeper, located at the group the darkest colored dots on this plot, located on the seam at the bottom right and top left. 
To more easily understand this orientation, here is a helpful visual.

![ScreenRecording2025-07-24at15 05 17-ezgif com-video-to-gif-converter (1)](https://github.com/user-attachments/assets/b60a156a-3be6-400e-b839-b096f589e123)

(You can mess around with this tool at https://scout.texasleaguers.com/spin)

This is the proper seam orientation for a sweeper, which aligns with the well documented orientation pitchers actually use for sweepers. 
Again, this does not point out the spin direction or gyro angle properly. Typically, a right handed sweeper has the spin axis point or "red dot" on the bottom right of the ball(pitcher's perspective).


You can also see that there is a clear relationship between this spin axis point and horizontal movement, on sliders specifically:

<img width="659" height="581" alt="Screenshot 2025-08-09 at 21 48 59" src="https://github.com/user-attachments/assets/de1a1405-9d00-4a53-994d-16924715efce" />

(Horizontal Break vs horizontal point location of the spin axis)

<img width="622" height="577" alt="Screenshot 2025-08-09 at 21 50 09" src="https://github.com/user-attachments/assets/7449a324-0526-405f-ab04-f643cd8897de" />

(Horizontal Break vs horizontal point location of the spin axis)

These two plots show that there is an optimal seam orientation for a sweeper. The actual numbers on the Y axis here are not as intuitive right away, but all they do is point to a location on that mollweide style plot.

While these do show a clear relationship of seam orientation to movement, I have a hard time calling this a linear relationship, despite what the plot may appear as. It really is just about finding the correct seam orientation. There is no "maximizing" any amount of orientation to achieve sweeper movement. It simply points out that there is an optimal seam orienation.

These two plots point out that the optimal sweeper orientation points are at about 110 degrees horizontal & about -45 degrees vertical, which points to that bottom right portion of the seam that tends to pick up lots of sweep, displayed earlier.



