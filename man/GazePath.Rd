\name{gazepath}
\alias{gazepath}
\alias{summary.gazepath}
\alias{plot.gazepath}
\alias{print.gazepath}
\title{
Transform Raw Eye-Tracking Data into Fixations and Saccades
}
\description{
This function transforms raw eye-tracking data into fixations and saccades using a non-parametric speed-based approach to do this on a trial basis. The method is especially useful when there are large differences in data quality between or within participants. Lower data quality results in more conservative thresholds, thereby controlling for noise levels.
}
\usage{
gazepath(data, x1, y1, x2 = NULL, y2 = NULL, distance, trial,
height_px, height_mm, width_px, width_mm, 
res_x = 1280, res_y = 1024, samplerate = 500, 
method = "Mould", posthoc = FALSE, thres_vel = 35, 
thres_dur = 100, min_dist = 250)

\method{summary}{gazepath}(object, ..., complete_only = FALSE)
\method{plot}{gazepath}(x, ..., i = 1)
\method{print}{gazepath}(x, ...)

}

\arguments{
  \item{data}{
The dataframe with at least the raw x- and y-coordinates, the distance to the screen in mm and a trial index.
}
  \item{x1}{
the column of the dataframe containing the x-coordinates
}
  \item{y1}{
the column of the dataframe containing the y-coordinates
}
  \item{x2}{
When tracking was binocular, the column of the dataframe containing the x-coordinates of the second eye
}
  \item{y2}{
When tracking was binocular, the column of the dataframe containing the y-coordinates of the second eye
}
  \item{distance}{
The column of the dataframe containing the distance in mm
}
  \item{trial}{
The column of the dataframe containing the trial or stimuli index
}
  \item{height_px}{
The height of the stimuli in pixels, can be a single value or a vector of length number of trials when stimuli differ in size per trial
}
  \item{height_mm}{
The height of the stimuli in mm, can be a single value or a vector of length number of trials when stimuli differ in size per trial
}
  \item{width_px}{
The width of the stimuli in pixels, can be a single value or a vector of length number of trials when stimuli differ in size per trial
}
  \item{width_mm}{
The height of the stimuli in pixels, can be a single value or a vector of length number of trials when stimuli differ in size per trial trials
}
  \item{res_x}{
The horizontal resolution of the monitor in pixels
}
  \item{res_y}{
The vertical resolution of the monitor in pixels
}
  \item{samplerate}{
The samplerate, e.g., 60 for a 60 Hz scanner, 500 for a 500 Hz scanner
}
  \item{method}{
Must be one of the following methods: Mould, Mould.all, MouldDur, Mould.fix, Tobii, Eyelink.

Mould estimates velocity threshold per trial and duration thresholds per person (recommended for high, > 250 Hz, samplerate and high quality data)

Mould.all estimates velocity and duration thresholds per person

MouldDur estimates velocity threshold per trial and uses the duration thresholds specified under 'thres_dur' (recommended for low, < 250 Hz, samplerate data and data of low quality)

Mould.fix estimates velocity thresholds per person and uses the duration thresholds specified under 'thres_dur'

Tobii derives fixations and saccades based on the algorithm described in the ClearView 2.7 manual

Eyelink classifies saccades as sequences of samples (> 4ms) that have a velocity over 35 deg/s, acceleration over 8000 deg/s^2 and defliction over 0.1 deg of visual angle. Fixations are the sequences of samples that are not saccades and pass the duration threshold of 100 ms
}
  \item{posthoc}{
TRUE or FALSE (default). When TRUE successive fixations close in space are merged.
}
  \item{thres_vel}{
The desired velocity threshold, will only be used for method 'Eyelink'
}
  \item{thres_dur}{
The desired duration threshold, will only be used for method 'Eyelink', 'MouldDur' and 'Mould.fix' 
}
  \item{min_dist}{
The minimal distance from the screen in mm, samples below this distance are considered unreliable and are omitted from further analysis.
}
  \item{object}{
  Object of class 'gazepath' can be used in the plot, summary and print function
  }
  \item{x}{
  Object of class 'gazepath' can be used in the plot, summary and print function
  }
  \item{i}{
  Argument of the plot function, refering to the trial to be plotted
  }
  \item{complete_only}{
  Argument (TRUE/FALSE) to indicate whether only fixations that have incoming out outgoing saccades and saccades that are between two fixations should be selected
  }
  \item{...}{
  Not used currently
  }
}
\details{
no details
}
\value{
gazepath returns a list of 16 items. plot() and summary() can be used to verify the procedure and obtain the fixations.

\item{1. Classifications}{List of length number of trials with a vector indicating whether a sample is classified as fixation 'f', saccade 's', unknown 'u' or NA}
\item{2. x-coordinates}{List of length number of trials with vectors of the x-coordinates of each trial}

\item{3. y-coordinates}{List of length number of trials with vectors of the y-coordinates of each trial}

\item{4. Method}{single value indicating what method was used to classify fixations and saccades}

\item{5. Robustness}{List of length number of trials with vectors containing the mean durations of usable data segments per trial. Higher values indicate better data quality}

\item{6. Presicion}{List of length number of trials with vectors containing the mean variances in point of gaze in degrees of visual angle per trial. Higher values indicate poorer data quality}

\item{7. Velocity thresholds}{List of length number of trials containing the velocity thresholds}

\item{8. Duration threshold}{The duration thresholds}

\item{9. Speed}{List of length number of trials with vectors containing the speed at each sample}

\item{10. Samplerate}{The samplerate}

\item{11. Head target distance}{List of length number of trials with vectors containing the distance to the screen in mm for each sample}

\item{12. Height in pixels}{Vector of length number of trials with the stimuli heights in pixels}

\item{13. Height in mm}{Vector of length number of trials with the stimuli heights in mm}

\item{14. Width in pixels}{Vector of length number of trials with the stimuli widths in pixels}

\item{15. Widtht in mm}{Vector of length number of trials with the stimuli widths in mm}

\item{16. Fixations and saccades per trial}{List of length number of trials with dataframes containing the fixations and saccades, see also summary()}


}
\references{
Mould, M. S., Foster, D. H., Amano, K., & Oakley, J. P. (2012). A simple nonparametric method for classifying eye fixations. Vision research, 57, 18-25.
}
\author{
Daan van Renswoude
}

\examples{
#### Example
data(screen)
data(eye_data)
example <- gazepath(eye_data, x1 = 1, y1 = 2, distance = 4, trial = 3, 
height_px = screen[,1], height_mm = screen[,2], width_px = screen[,3], width_mm = screen[,4], 
method = 'Mould', samplerate = 500)

## Use different i's to check the estimated fixations per trial
i <- 1
plot(example, i = i)

## The estimated fixations, orderded per trial
summary(example, complete_only = FALSE)
}
\keyword{ fixations }
\keyword{ saccades }
